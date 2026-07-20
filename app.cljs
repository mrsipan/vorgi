(require '["https://esm.sh/orga" :as orga])
(require '["https://unpkg.com/squint-cljs/src/squint/string.js" :as str])

;; ============================================================
;; Utility: escape HTML
;; ============================================================
(defn escape-html [s]
  (if s
    (-> s
        (.replace "&" "&amp;")
        (.replace "<" "&lt;")
        (.replace ">" "&gt;")
        (.replace "\"" "&quot;"))
    ""))

;; ============================================================
;; Org-mode AST to HTML converter
;; ============================================================
(defn node->html [node]
  (let [t (.-type node)]
    (case t
      "document" (let [children (.-children node)]
                   (str "<div class='org-doc'>"
                        (.join (.map children (fn [c] (node->html c))) "")
                        "</div>"))
      "section" (let [children (.-children node)]
                  (str "<div class='org-section org-level-" (.-level node) "'>"
                       (.join (.map children (fn [c] (node->html c))) "")
                       "</div>"))
      "headline" (let [level (.-level node)
                       children (.-children node)
                       filtered (.filter children (fn [c] (not= "stars" (.-type c))))]
                   (str "<h" level " class='org-headline'>"
                        (.join (.map filtered (fn [c] (node->html c))) "")
                        "</h" level ">"))
      "paragraph" (let [children (.-children node)]
                    (str "<p>"
                         (.join (.map children (fn [c] (node->html c))) "")
                         "</p>"))
      "text" (let [style (.-style node)
                   val (or (.-value node) "")]
               (cond (= style "italic") (str "<em>" val "</em>")
                     (= style "bold") (str "<strong>" val "</strong>")
                     (= style "code") (str "<code>" val "</code>")
                     (= style "verbatim") (str "<code>" val "</code>")
                     :else val))
      "list" (let [children (.-children node)]
               (str "<ul>"
                    (.join (.map children (fn [c] (node->html c))) "")
                    "</ul>"))
      "list.item" (let [children (.-children node)
                        f1 (.filter children (fn [c] (not= "list.item.bullet" (.-type c))))
                        f2 (.filter f1 (fn [c] (not= "newline" (.-type c))))]
                    (str "<li>"
                         (.join (.map f2 (fn [c] (node->html c))) "")
                         "</li>"))
      "stars" ""
      "newline" ""
      "list.item.bullet" ""
      (str "<!-- unknown: " t " -->"))))

(defn org-to-html [text]
  (if (or (nil? text) (= "" text))
    ""
    (try
      (let [ast (.parse orga text)]
        (node->html ast))
      (catch js/Error e
        (str "<pre>Parse error: " (.-message e) "</pre>")))))

;; ============================================================
;; Vim Editor State
;; ============================================================
(def app-state
  (atom {:text "Hello from Vim!\nThis is the editor.\n\n* Org mode heading\nSome text here"
         :mode "normal"
         :cursor {:row 0 :col 0}
         :visual-start nil       ;; anchor cursor position when visual mode entered
         :visual-type nil        ;; nil, "char", "line", "block"
         :history []
         :message ""
         :operator nil
         :text-object-type nil}))

(defn get-lines [s]
  (.split (:text s) "\n"))

(defn clamp [v mn mx]
  (max mn (min v mx)))

(defn line-length [lines row]
  (if (and lines (< row (.-length lines)))
    (.-length (aget lines row))
    0))

;; State helpers
(defn save-history [s]
  (assoc s :history (conj (or (:history s) []) (:text s))))

;; ============================================================
;; Vim Commands (pure functions returning new state)
;; ============================================================
(defn cmd-move-cursor [s dr dc]
  (let [lines (get-lines s)
        row (clamp (+ (:row (:cursor s)) dr) 0 (dec (.-length lines)))
        col (clamp (+ (:col (:cursor s)) dc) 0 (line-length lines row))]
    (assoc s :cursor {:row row :col col})))

(defn replace-line [lines row new-text]
  (.join (.concat (.slice lines 0 row)
                  #js [new-text]
                  (.slice lines (inc row))) "\n"))

(defn cmd-insert-text [s text]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)
        new-line (str (.slice line 0 col) text (.slice line col))]
    (-> s
        (save-history)
        (assoc :text (replace-line lines row new-line))
        (assoc-in [:cursor :col] (+ col (.-length text))))))

(defn cmd-insert-newline [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)
        before (.slice line 0 col)
        after (.slice line col)
        new-lines (.concat (.slice lines 0 row)
                           #js [before after]
                           (.slice lines (inc row)))]
    (-> s
        (save-history)
        (assoc :text (.join new-lines "\n"))
        (assoc :cursor {:row (inc row) :col 0}))))

(defn cmd-insert-after [s]
  "Move cursor right one column if possible, then enter insert mode for a key."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (< col (.-length line))
      (-> s
          (assoc :mode "insert")
          (assoc-in [:cursor :col] (inc col)))
      (assoc s :mode "insert"))))

(defn cmd-delete-char [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (< col (.-length line))
      (-> s
          (save-history)
          (assoc :text (replace-line lines row
                        (str (.slice line 0 col) (.slice line (inc col))))))
      s)))

(defn cmd-delete-backward [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (> col 0)
      ;; Delete char before cursor on same line
      (-> s
          (save-history)
          (assoc :text (replace-line lines row
                        (str (.slice line 0 (dec col)) (.slice line col))))
          (assoc-in [:cursor :col] (dec col)))
      ;; At column 0: join with previous line
      (if (> row 0)
        (let [prev-line (aget lines (dec row))
              new-text (str prev-line line)
              new-lines (.concat (.slice lines 0 (dec row))
                                 #js [new-text]
                                 (.slice lines (inc row)))]
          (-> s
              (save-history)
              (assoc :text (.join new-lines "\n"))
              (assoc :cursor {:row (dec row) :col (.-length prev-line)})))
        s))))

(defn cmd-delete-word-backward [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (> col 0)
      (let [re #"[a-zA-Z0-9_]"
            prev-char-pos (dec col)
            ;; Skip non-word chars before cursor
            start-pos (loop [c prev-char-pos]
                        (if (and (>= c 0)
                                 (not (re-find re (.slice line c (inc c)))))
                          (recur (dec c))
                          c))
            ;; Find start of word
            word-start (loop [c start-pos]
                         (if (and (> c 0)
                                  (re-find re (.slice line (dec c) c)))
                           (recur (dec c))
                           c))]
        (-> s
            (save-history)
            (assoc :text (replace-line lines row
                          (str (.slice line 0 word-start) (.slice line col))))
            (assoc-in [:cursor :col] word-start)))
      (if (> row 0)
        (let [prev-line (aget lines (dec row))
              new-text (str prev-line line)
              new-lines (.concat (.slice lines 0 (dec row))
                                 #js [new-text]
                                 (.slice lines (inc row)))]
          (-> s
              (save-history)
              (assoc :text (.join new-lines "\n"))
              (assoc :cursor {:row (dec row) :col (.-length prev-line)})))
        s))))

(defn cmd-delete-line [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        line (aget lines row)]
    (if (= 1 (.-length lines))
      (-> s (save-history) (assoc :clipboard line) (assoc :text "" :cursor {:row 0 :col 0}))
      (let [new-lines (.filter lines (fn [_ idx] (not= idx (:row (:cursor s)))))
            new-row (min (:row (:cursor s)) (dec (.-length new-lines)))]
        (-> s
            (save-history)
            (assoc :clipboard line)
            (assoc :text (.join new-lines "\n"))
            (assoc :cursor {:row new-row :col 0}))))))

(defn cmd-delete-to-end-of-line [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (< col (.-length line))
      (let [deleted (.slice line col)
            new-line (.slice line 0 col)]
        (-> s
            save-history
            (assoc :clipboard deleted)
            (assoc :text (replace-line lines row new-line))))
      s)))

(defn cmd-yank-line [s]
  "Yank (copy) the current line into clipboard without modifying text."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        line (aget lines row)]
    (assoc s :clipboard line :message "Yanked")))

(defn cmd-yank-to-end-of-line [s]
  "Yank (copy) from cursor column to end of current line."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (< col (.-length line))
      (assoc s :clipboard (.slice line col) :message "Yanked")
      (assoc s :message ""))))

(defn cmd-paste-after [s]
  "Insert clipboard text after cursor on current line, move cursor to end."
  (let [clip (:clipboard s)]
    (if (nil? clip)
      (assoc s :message "No clipboard content")
      (let [lines (get-lines s)
            row (:row (:cursor s))
            col (:col (:cursor s))
            line (aget lines row)
            before (.slice line 0 (inc col))
            after (.slice line (inc col))
            new-line (str before clip after)
            new-col (+ (inc col) (.-length clip))]
        (-> s
            (save-history)
            (assoc :text (replace-line lines row new-line))
            (assoc :cursor {:row row :col new-col})
            (assoc :message ""))))))

(defn cmd-paste-before [s]
  "Insert clipboard text before cursor on current line, move cursor to end."
  (let [clip (:clipboard s)]
    (if (nil? clip)
      (assoc s :message "No clipboard content")
      (let [lines (get-lines s)
            row (:row (:cursor s))
            col (:col (:cursor s))
            line (aget lines row)
            before (.slice line 0 col)
            after (.slice line col)
            new-line (str before clip after)
            new-col (+ col (.-length clip))]
        (-> s
            (save-history)
            (assoc :text (replace-line lines row new-line))
            (assoc :cursor {:row row :col new-col})
            (assoc :message ""))))))

;; ============================================================
;; Visual Mode
;; ============================================================
(defn cmd-enter-visual [s type]
  "Enter visual mode of given type (char, line, or block). Sets the anchor
   to the current cursor position."
  (assoc s
         :mode "visual"
         :visual-type type
         :visual-start (:cursor s)))

(defn visual-bounds [s]
  "Return the normalized bounds of the visual selection.
   For 'block' type, returns the rectangular region covering all lines
   between start and end rows, and all columns between start and end cols.
   Returns {:start {:row r :col c} :end {:row r :col c}}"
  (let [cursor (:cursor s)
        vstart (:visual-start s)
        vtype (:visual-type s)]
    (if (nil? vstart)
      {:start cursor :end cursor}
      (case vtype
        "line"
        (let [sr (:row vstart)
              er (:row cursor)
              lines (.split (:text s) "\n")
              last-line-len (if (< er sr)
                              (.-length (aget lines sr))
                              (.-length (aget lines er)))]
          (if (< er sr)
            {:start {:row er :col 0}
             :end {:row sr :col last-line-len}}
            {:start {:row sr :col 0}
             :end {:row er :col last-line-len}}))

        "block"
        (let [sr (:row vstart)
              sc (:col vstart)
              er (:row cursor)
              ec (:col cursor)]
          {:start {:row (min sr er) :col (min sc ec)}
           :end {:row (max sr er) :col (max sc ec)}})

        ;; "char" (default)
        (let [sr (:row vstart)
              sc (:col vstart)
              er (:row cursor)
              ec (:col cursor)]
          (if (or (< er sr) (and (= er sr) (< ec sc)))
            {:start {:row er :col ec}
             :end {:row sr :col sc}}
            {:start {:row sr :col sc}
             :end {:row er :col ec}}))))))

(defn get-selected-text [s]
  "Return the text currently selected in visual mode."
  (let [bounds (visual-bounds s)
        start (:start bounds)
        end (:end bounds)
        sr (:row start)
        sc (:col start)
        er (:row end)
        ec (:col end)
        vtype (:visual-type s)
        lines (.split (:text s) "\n")]
    (case vtype
      "block"
      ;; For block selection, build a rectangular region
      ;; Each row is trimmed to the character in [min-col, max-col+1) range inclusive
      (let [rows (vec (range sr (inc er)))
            selected-lines (.map rows (fn [row]
                                        (let [line (aget lines row)]
                                          (.slice line sc (inc ec)))))]
        (.join selected-lines "\n"))

      ;; "char" and "line" use the existing contiguous range logic
      (get-text-range (:text s) sr sc er ec))))

(defn delete-block-range [text start-row start-col end-row end-col]
  "Delete a rectangular (block) region from the text.
   For each row in start-row to end-row, remove columns start-col to end-col inclusive.
   Returns the new text."
  (let [lines (.split text "\n")
        new-lines (.map lines (fn [line row]
                                (if (and (>= row start-row) (<= row end-row))
                                  (if (>= start-col (.-length line))
                                    line
                                    (str (.slice line 0 start-col)
                                         (.slice line (inc end-col))))
                                  line)))]
    (.join new-lines "\n")))

(defn cmd-delete-selection [s]
  "Delete the visual selection. For block mode, delete the rectangular region."
  (let [bounds (visual-bounds s)
        start (:start bounds)
        end (:end bounds)
        sr (:row start)
        sc (:col start)
        er (:row end)
        ec (:col end)
        vtype (:visual-type s)]
    (case vtype
      "block"
      (let [new-text (delete-block-range (:text s) sr sc er ec)]
        (-> s
            save-history
            (assoc :text new-text)
            (assoc :mode "normal")
            (assoc :visual-start nil)
            (assoc :visual-type nil)
            (assoc :cursor {:row sr :col sc})))

      ;; "char" or "line" - use existing delete-range
      (let [new-text (delete-range (:text s) sr sc er ec)]
        (-> s
            save-history
            (assoc :text new-text)
            (assoc :mode "normal")
            (assoc :visual-start nil)
            (assoc :visual-type nil)
            (assoc :cursor {:row sr :col sc}))))))

(defn cmd-yank-selection [s]
  "Yank the visual selection into clipboard."
  (let [yanked-text (get-selected-text s)]
    (-> s
        (assoc :clipboard yanked-text)
        (assoc :mode "normal")
        (assoc :visual-start nil)
        (assoc :visual-type nil)
        (assoc :message "Yanked"))))

(defn cmd-undo [s]
  (let [h (:history s)]
    (if (pos? (.-length h))
      (-> s
          (assoc :text (last h))
          (assoc :history (.slice h 0 (dec (.-length h))))
          (assoc :cursor {:row 0 :col 0}))
      (assoc s :message "No undo history"))))

(defn cmd-open-below [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        new-lines (.concat (.slice lines 0 (inc row))
                           #js [""]
                           (.slice lines (inc row)))]
    (-> s
        (save-history)
        (assoc :text (.join new-lines "\n"))
        (assoc :cursor {:row (inc row) :col 0})
        (assoc :mode "insert"))))

(defn cmd-open-above [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        new-lines (.concat (.slice lines 0 row)
                           #js [""]
                           (.slice lines row))]
    (-> s
        (save-history)
        (assoc :text (.join new-lines "\n"))
        (assoc :cursor {:row row :col 0})
        (assoc :mode "insert"))))

(defn cmd-move-word-forward [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (>= col (dec (.-length line)))
      (if (< row (dec (.-length lines)))
        (assoc s :cursor {:row (inc row) :col 0})
        s)
      (let [;; skip non-word chars
            re #"[a-zA-Z0-9_]"
            start-col (loop [c col]
                        (if (and (< c (.-length line))
                                 (not (re-find re (.slice line c (inc c)))))
                          (recur (inc c))
                          c))
            ;; find end of word
            next-col (loop [c start-col]
                       (if (and (< c (.-length line))
                                (re-find re (.slice line c (inc c))))
                         (recur (inc c))
                         c))]
        (if (and (> next-col start-col) (<= next-col (.-length line)))
          (assoc s :cursor {:row row :col next-col})
          (if (< row (dec (.-length lines)))
            (assoc s :cursor {:row (inc row) :col 0})
            (assoc s :cursor {:row row :col (.-length line)})))))))

(defn cmd-move-word-backward [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (<= col 0)
      (if (> row 0)
        (let [prev-line (aget lines (dec row))]
          (assoc s :cursor {:row (dec row) :col (.-length prev-line)}))
        s)
      (let [re #"[a-zA-Z0-9_]"
            prev-char-pos (dec col)
            start-pos (loop [c prev-char-pos]
                        (if (and (>= c 0)
                                 (not (re-find re (.slice line c (inc c)))))
                          (recur (dec c))
                          c))
            word-start (loop [c start-pos]
                         (if (and (> c 0)
                                  (re-find re (.slice line (dec c) c)))
                           (recur (dec c))
                           c))]
        (if (>= word-start 0)
          (assoc s :cursor {:row row :col word-start})
          (assoc s :cursor {:row row :col 0}))))))

;; ============================================================
;; Character find motions (f, F, t, T, ;, ,)
;; ============================================================
(defn cmd-find-char-forward [s char]
  "Move cursor to next occurrence of char on current line search from cursor plus 1."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (inc col)
            found (.indexOf line char start-pos)]
        (if (>= found 0)
          (assoc-in s [:cursor :col] found)
          s))
      s)))

(defn cmd-find-char-backward [s char]
  "Move cursor to previous occurrence of char on current line search from cursor minus 1."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (dec col)
            found (.lastIndexOf line char start-pos)]
        (if (>= found 0)
          (assoc-in s [:cursor :col] found)
          s))
      s)))

(defn cmd-find-till-forward [s char]
  "Move cursor to just before the next occurrence of char till."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (inc col)
            found (.indexOf line char start-pos)]
        (if (> found 0)
          (assoc-in s [:cursor :col] (dec found))
          s))
      s)))

(defn cmd-find-till-backward [s char]
  "Move cursor to just after the previous occurrence of char backward till."
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (dec col)
            found (.lastIndexOf line char start-pos)]
        (if (>= found 0)
          (let [new-col (inc found)]
            (if (<= new-col col)
              (assoc-in s [:cursor :col] new-col)
              s))
          s))
      s)))

;; ============================================================
;; Text range operations
;; ============================================================
(defn delete-range [text start-row start-col end-row end-col]
  (let [lines (.split text "\n")]
    (if (= start-row end-row)
      (let [line (aget lines start-row)
            new-line (str (.slice line 0 start-col) (.slice line end-col))]
        (.join (.concat (.slice lines 0 start-row)
                        #js [new-line]
                        (.slice lines (inc start-row))) "\n"))
      (let [first-line (.slice (aget lines start-row) 0 start-col)
            last-line (.slice (aget lines end-row) end-col)
            new-lines (.concat (.slice lines 0 start-row)
                               #js [(str first-line last-line)]
                               (.slice lines (inc end-row)))]
        (.join new-lines "\n")))))

(defn get-text-range [text start-row start-col end-row end-col]
  (let [lines (.split text "\n")]
    (if (= start-row end-row)
      (.slice (aget lines start-row) start-col end-col)
      (let [first-line (.slice (aget lines start-row) start-col)
            middle-lines (.slice lines (inc start-row) end-row)
            last-line (.slice (aget lines end-row) 0 end-col)]
        (.join (.concat #js [first-line] middle-lines #js [last-line]) "\n")))))

;; ============================================================
;; Motion commands (used in operator-pending mode)
;; ============================================================
(defn motion-key? [k]
  (contains? #{"h" "j" "k" "l" "w" "b" "e" "0" "$"} k))

(defn apply-motion [s key]
  (case key
    "h" (cmd-move-cursor s 0 -1)
    "j" (cmd-move-cursor s 1 0)
    "k" (cmd-move-cursor s -1 0)
    "l" (cmd-move-cursor s 0 1)
    "w" (cmd-move-word-forward s)
    "b" (cmd-move-word-backward s)
    "0" (assoc-in s [:cursor :col] 0)
    "$" (let [lines (.split (:text s) "\n")
              row (:row (:cursor s))]
          (assoc-in s [:cursor :col] (.-length (aget lines row))))
    "e" (let [lines (.split (:text s) "\n")
              re #"[a-zA-Z0-9_]"
              row (:row (:cursor s))
              col (:col (:cursor s))
              line (aget lines row)]
          (if (>= col (dec (.-length line)))
            (if (< row (dec (.-length lines)))
              (assoc s :cursor {:row (inc row) :col 0})
              (assoc s :cursor {:row row :col (.-length line)}))
            (let [next-col (loop [c (inc col)]
                             (if (and (< c (.-length line))
                                      (re-find re (.slice line c (inc c))))
                               (recur (inc c))
                               c))]
              (assoc s :cursor {:row row :col next-col}))))
    s))

;; ============================================================
;; Operator application (for motion-based operators: d, c, y)
;; ============================================================
(defn apply-operator [s operator motion-s]
  (let [text (:text s)
        cursor (:cursor s)
        start-row (:row cursor)
        start-col (:col cursor)
        end-cursor (:cursor motion-s)
        end-row (:row end-cursor)
        end-col (:col end-cursor)
        ;; Normalize: start < end
        [sr sc er ec] (if (or (< end-row start-row)
                              (and (= end-row start-row) (< end-col start-col)))
                        [end-row end-col start-row start-col]
                        [start-row start-col end-row end-col])]
    (case operator
      "delete" (let [new-text (delete-range text sr sc er ec)]
                 (-> s save-history
                     (assoc :text new-text)
                     (assoc :cursor {:row sr :col sc})
                     (assoc :operator nil)))
      "yank" (let [yanked-text (get-text-range text sr sc er ec)]
               (assoc s :clipboard yanked-text :operator nil :message "Yanked"))
      "change" (let [new-text (delete-range text sr sc er ec)]
                 (-> s save-history
                     (assoc :text new-text)
                     (assoc :cursor {:row sr :col sc})
                     (assoc :mode "insert")
                     (assoc :operator nil)))
      (assoc s :operator nil))))

;; ============================================================
;; Text Object Functions
;; ============================================================
(defn word-bounds [text cursor]
  (let [lines (.split text "\n")
        row (:row cursor)
        col (:col cursor)
        line (aget lines row)]
    (if (or (nil? line) (>= col (.-length line)))
      (let [len (.-length line)]
        {:start {:row row :col len} :end {:row row :col len}})
      (let [re #"[a-zA-Z0-9_]"
            ch (.slice line col (inc col))
            is-word? (boolean (re-find re ch))]
        (if is-word?
          (let [word-start (loop [c col]
                             (if (and (> c 0) (re-find re (.slice line (dec c) c)))
                               (recur (dec c))
                               c))
                word-end (loop [c col]
                           (if (and (< c (.-length line)) (re-find re (.slice line c (inc c))))
                             (recur (inc c))
                             c))]
            {:start {:row row :col word-start} :end {:row row :col word-end}})
          (let [nonword-start (loop [c col]
                                (if (and (> c 0) (not (re-find re (.slice line (dec c) c))))
                                  (recur (dec c))
                                  c))
                nonword-end (loop [c col]
                              (if (and (< c (.-length line)) (not (re-find re (.slice line c (inc c)))))
                                (recur (inc c))
                                c))]
            {:start {:row row :col nonword-start} :end {:row row :col nonword-end}}))))))

(defn find-quote-bounds [text cursor]
  (let [lines (.split text "\n")
        row (:row cursor)
        col (:col cursor)
        line (aget lines row)]
    (if (or (nil? line) (>= col (.-length line)))
      {:start {:row row :col (.-length line)} :end {:row row :col (.-length line)} :quote nil}
      (let [ch (.slice line col (inc col))
            quotes #{"\"" "'" "`"}
            quote (if (contains? quotes ch) ch nil)]
        (if quote
          ;; Cursor is ON a quote character
          (let [open-pos (loop [c (dec col)]
                           (if (< c 0) nil
                               (if (= (.slice line c (inc c)) quote) c
                                   (recur (dec c)))))
                close-pos (loop [c (inc col)]
                            (if (>= c (.-length line)) nil
                                (if (= (.slice line c (inc c)) quote) c
                                    (recur (inc c)))))]
            (if (and open-pos close-pos)
              {:start {:row row :col open-pos} :end {:row row :col (inc close-pos)} :quote quote}
              (if open-pos
                {:start {:row row :col open-pos} :end {:row row :col (inc col)} :quote quote}
                (if close-pos
                  {:start {:row row :col col} :end {:row row :col (inc close-pos)} :quote quote}
                  {:start {:row row :col col} :end {:row row :col (inc col)} :quote quote}))))
          ;; Cursor is NOT on a quote — search backward/forward
          (let [open-pos (loop [c col]
                           (if (< c 0) nil
                               (let [cc (.slice line c (inc c))]
                                 (if (contains? quotes cc) c
                                     (recur (dec c))))))
                close-pos (when open-pos
                            (let [q (.slice line open-pos (inc open-pos))]
                              (loop [c (inc open-pos)]
                                (if (>= c (.-length line)) nil
                                    (if (= (.slice line c (inc c)) q) c
                                        (recur (inc c)))))))]
            (if (and open-pos close-pos)
              {:start {:row row :col open-pos} :end {:row row :col (inc close-pos)} :quote (.slice line open-pos (inc open-pos))}
              (let [fwd-open (loop [c col]
                               (if (>= c (.-length line)) nil
                                   (let [cc (.slice line c (inc c))]
                                     (if (contains? quotes cc) c
                                         (recur (inc c))))))
                    fwd-close (when fwd-open
                                (let [q (.slice line fwd-open (inc fwd-open))]
                                  (loop [c (inc fwd-open)]
                                    (if (>= c (.-length line)) nil
                                        (if (= (.slice line c (inc c)) q) c
                                            (recur (inc c)))))))]
                (if (and fwd-open fwd-close)
                  {:start {:row row :col fwd-open} :end {:row row :col (inc fwd-close)} :quote (.slice line fwd-open (inc fwd-open))}
                  {:start {:row row :col col} :end {:row row :col col} :quote nil})))))))))

(defn find-bracket-bounds [text cursor]
  (let [lines (.split text "\n")
        row (:row cursor)
        col (:col cursor)
        line (aget lines row)]
    (if (or (nil? line) (>= col (.-length line)))
      {:start {:row row :col (.-length line)} :end {:row row :col (.-length line)} :bracket nil}
      (let [ch (.slice line col (inc col))
            pairs {"(" ")" ")" "(" "[" "]" "]" "[" "{" "}" "}" "{"}
            openers #{"(" "[" "{"}
            closers #{")" "]" "}"}
            bracket-type (cond (contains? openers ch) "open"
                               (contains? closers ch) "close"
                               :else nil)]
        (if bracket-type
          ;; Cursor is ON a bracket
          (let [target (get pairs ch)]
            (if (= bracket-type "open")
              (let [result (loop [i (inc col) depth 1]
                             (if (>= i (.-length line)) nil
                                 (let [c (.slice line i (inc i))]
                                   (cond (= c ch) (recur (inc i) (inc depth))
                                         (= c target) (if (= depth 1) i
                                                         (recur (inc i) (dec depth)))
                                         :else (recur (inc i) depth)))))]
                (if result
                  {:start {:row row :col col} :end {:row row :col (inc result)} :bracket ch}
                  {:start {:row row :col col} :end {:row row :col (inc col)} :bracket ch}))
              ;; close bracket
              (let [result (loop [i (dec col) depth 1]
                             (if (< i 0) nil
                                 (let [c (.slice line i (inc i))]
                                   (cond (= c ch) (recur (dec i) (inc depth))
                                         (= c target) (if (= depth 1) i
                                                         (recur (dec i) (dec depth)))
                                         :else (recur (dec i) depth)))))]
                (if result
                  {:start {:row row :col result} :end {:row row :col (inc col)} :bracket target}
                  {:start {:row row :col col} :end {:row row :col (inc col)} :bracket ch}))))
          ;; Cursor is NOT on a bracket
          (let [open-pos (loop [c col]
                           (if (< c 0) nil
                               (let [cc (.slice line c (inc c))]
                                 (if (contains? openers cc) c
                                     (recur (dec c))))))]
            (if open-pos
              (let [open-ch (.slice line open-pos (inc open-pos))
                    close-target (get pairs open-ch)
                    close-pos (loop [i (inc open-pos) depth 1]
                                (if (>= i (.-length line)) nil
                                    (let [c (.slice line i (inc i))]
                                      (cond (= c open-ch) (recur (inc i) (inc depth))
                                            (= c close-target) (if (= depth 1) i
                                                                  (recur (inc i) (dec depth)))
                                            :else (recur (inc i) depth)))))]
                (if close-pos
                  {:start {:row row :col open-pos} :end {:row row :col (inc close-pos)} :bracket open-ch}
                  (let [fwd-open (loop [c col]
                                   (if (>= c (.-length line)) nil
                                       (let [cc (.slice line c (inc c))]
                                         (if (contains? openers cc) c
                                             (recur (inc c))))))]
                    (if fwd-open
                      (let [fwd-open-ch (.slice line fwd-open (inc fwd-open))
                            fwd-close-target (get pairs fwd-open-ch)
                            fwd-close-pos (loop [i (inc fwd-open) depth 1]
                                            (if (>= i (.-length line)) nil
                                                (let [c (.slice line i (inc i))]
                                                  (cond (= c fwd-open-ch) (recur (inc i) (inc depth))
                                                        (= c fwd-close-target) (if (= depth 1) i
                                                                                 (recur (inc i) (dec depth)))
                                                        :else (recur (inc i) depth)))))]
                        (if fwd-close-pos
                          {:start {:row row :col fwd-open} :end {:row row :col (inc fwd-close-pos)} :bracket fwd-open-ch}
                          {:start {:row row :col col} :end {:row row :col col} :bracket nil}))
                      {:start {:row row :col col} :end {:row row :col col} :bracket nil}))))
              (let [fwd-open (loop [c col]
                               (if (>= c (.-length line)) nil
                                   (let [cc (.slice line c (inc c))]
                                     (if (contains? openers cc) c
                                         (recur (inc c))))))]
                (if fwd-open
                  (let [fwd-open-ch (.slice line fwd-open (inc fwd-open))
                        fwd-close-target (get pairs fwd-open-ch)
                        fwd-close-pos (loop [i (inc fwd-open) depth 1]
                                        (if (>= i (.-length line)) nil
                                            (let [c (.slice line i (inc i))]
                                              (cond (= c fwd-open-ch) (recur (inc i) (inc depth))
                                                    (= c fwd-close-target) (if (= depth 1) i
                                                                             (recur (inc i) (dec depth)))
                                                    :else (recur (inc i) depth)))))]
                    (if fwd-close-pos
                      {:start {:row row :col fwd-open} :end {:row row :col (inc fwd-close-pos)} :bracket fwd-open-ch}
                      {:start {:row row :col col} :end {:row row :col col} :bracket nil}))
                  {:start {:row row :col col} :end {:row row :col col} :bracket nil})))))))))

(defn text-object-bounds [text cursor type key]
  (cond
    (= key "w")
    (let [bounds (word-bounds text cursor)
          start-col (:col (:start bounds))
          end-col (:col (:end bounds))]
      (if (= type "inner")
        bounds
        ;; "around" — include surrounding whitespace
        (let [lines (.split text "\n")
              row (:row cursor)
              line (aget lines row)]
          (if (and (< end-col (.-length line)) (= " " (.slice line end-col (inc end-col))))
            {:start (:start bounds) :end {:row row :col (inc end-col)}}
            (if (and (> start-col 0) (= " " (.slice line (dec start-col) start-col)))
              {:start {:row row :col (dec start-col)} :end (:end bounds)}
              bounds)))))

    (contains? #{"\"" "'" "`"} key)
    (let [qb (find-quote-bounds text cursor)]
      (if (:quote qb)
        (if (= type "inner")
          {:start {:row (:row (:start qb)) :col (inc (:col (:start qb)))}
           :end {:row (:row (:end qb)) :col (dec (:col (:end qb)))}}
          qb)
        {:start cursor :end cursor}))

    :else
    (let [bb (find-bracket-bounds text cursor)]
      (if (:bracket bb)
        (if (= type "inner")
          {:start {:row (:row (:start bb)) :col (inc (:col (:start bb)))}
           :end {:row (:row (:end bb)) :col (dec (:col (:end bb)))}}
          bb)
        {:start cursor :end cursor}))))

(defn apply-operator-to-bounds [s operator bounds]
  (let [text (:text s)
        start (:start bounds)
        end (:end bounds)
        start-row (:row start)
        start-col (:col start)
        end-row (:row end)
        end-col (:col end)]
    (case operator
      "delete" (let [new-text (delete-range text start-row start-col end-row end-col)]
                 (-> s save-history
                     (assoc :text new-text)
                     (assoc :cursor {:row start-row :col start-col})
                     (assoc :operator nil)))
      "yank" (let [yanked-text (get-text-range text start-row start-col end-row end-col)]
               (assoc s :clipboard yanked-text :operator nil :message "Yanked"))
      "change" (let [new-text (delete-range text start-row start-col end-row end-col)]
                 (-> s save-history
                     (assoc :text new-text)
                     (assoc :cursor {:row start-row :col start-col})
                     (assoc :mode "insert")
                     (assoc :operator nil)))
      (assoc s :operator nil))))

;; ============================================================
;; Surround operations (vim-surround)
;; ============================================================
(def surround-pairs {"(" ")" ")" "(" "[" "]" "]" "[" "{" "}" "}" "{" "\"" "\"" "'" "'" "`" "`"})

(defn cmd-delete-surround [s char]
  "Delete the surrounding pair of char around cursor on current line."
  (let [lines (.split (:text s) "\n")
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)
        openers #{"(" "[" "{"}
        closers #{")" "]" "}"}
        quotes #{"\"" "'" "`"}]
    (if (contains? quotes char)
      ;; Quote handling: find nearest pair around cursor
      (let [open-pos (loop [c col]
                       (if (< c 0) nil
                           (if (= (.slice line c (inc c)) char) c
                               (recur (dec c)))))
            close-pos (when open-pos
                        (loop [c (inc open-pos)]
                          (if (>= c (.-length line)) nil
                              (if (= (.slice line c (inc c)) char) c
                                  (recur (inc c))))))]
        (if (and open-pos close-pos)
          (let [new-line (str (.slice line 0 open-pos) (.slice line (inc open-pos) close-pos) (.slice line (inc close-pos)))
                new-text (replace-line lines row new-line)]
            (assoc s :text new-text :cursor {:row row :col (max 0 (dec col))}))
          (assoc s :message "No matching surround found")))
      ;; Bracket handling: char can be any bracket in the pair
      (let [[opener closer] (if (contains? openers char)
                              [char (get surround-pairs char)]
                              [(get surround-pairs char) char])]
        ;; Find the nearest opener backward from cursor
        (let [open-pos (loop [c (dec col)]
                         (if (< c 0) nil
                             (if (= (.slice line c (inc c)) opener) c
                                 (recur (dec c)))))]
          (if open-pos
            ;; Find matching closer
            (let [close-pos (loop [i (inc open-pos) depth 1]
                              (if (>= i (.-length line)) nil
                                  (let [c (.slice line i (inc i))]
                                    (cond (= c opener) (recur (inc i) (inc depth))
                                          (= c closer) (if (= depth 1) i
                                                          (recur (inc i) (dec depth)))
                                          :else (recur (inc i) depth)))))]
              (if close-pos
                (let [new-line (str (.slice line 0 open-pos) (.slice line (inc open-pos) close-pos) (.slice line (inc close-pos)))
                      new-text (replace-line lines row new-line)]
                  (assoc s :text new-text :cursor {:row row :col (max 0 (dec col))}))
                (assoc s :message "No matching surround found")))
            ;; Try forward
            (let [fwd-open (loop [c col]
                             (if (>= c (.-length line)) nil
                                 (if (= (.slice line c (inc c)) opener) c
                                     (recur (inc c)))))]
              (if fwd-open
                (let [close-pos (loop [i (inc fwd-open) depth 1]
                                  (if (>= i (.-length line)) nil
                                      (let [c (.slice line i (inc i))]
                                        (cond (= c opener) (recur (inc i) (inc depth))
                                              (= c closer) (if (= depth 1) i
                                                              (recur (inc i) (dec depth)))
                                              :else (recur (inc i) depth)))))]
                  (if close-pos
                    (let [new-line (str (.slice line 0 fwd-open) (.slice line (inc fwd-open) close-pos) (.slice line (inc close-pos)))
                          new-text (replace-line lines row new-line)]
                      (assoc s :text new-text :cursor {:row row :col (max 0 (min col (dec col)))}))
                    (assoc s :message "No matching surround found")))
                (assoc s :message "No matching surround found")))))))))

;; ============================================================
;; Keyboard event dispatcher
;; ============================================================
(def last-key-time (atom 0))
(def pending-d (atom false))
(def pending-g (atom false))
(def pending-find (atom nil))   ;; nil or {:type "f"/"F"/"t"/"T" :dir :forward/:backward}
(def last-find (atom nil))      ;; nil or {:char "x" :type "f" :dir :forward}


(defn handle-key [e]
  (let [key (.-key e)
        ctrl (.-ctrlKey e)
        state @app-state
        mode (:mode state)]
    (.preventDefault e)
    (if (or (= key "Escape") (and ctrl (= key (str (char 91)))))
      (do (reset! pending-d false) (reset! pending-g false) (reset! pending-find nil)
          (if (= mode "visual")
            (do (swap! app-state assoc :mode "normal" :message "" :visual-start nil :visual-type nil :operator nil :text-object-type nil) (render))
            (do (swap! app-state assoc :mode "normal" :message "" :operator nil :text-object-type nil) (render))))
      (if (= mode "insert")
        (do (cond
              (and ctrl (= key "w")) (swap! app-state cmd-delete-word-backward)
              (= key "Enter") (swap! app-state cmd-insert-newline)
              (= key "Backspace") (swap! app-state cmd-delete-backward)
              (= (.-length key) 1) (swap! app-state cmd-insert-text key))
            (render))
        (if (= mode "command")
          (do (case key "w" (swap! app-state assoc :message "Saved - simulated") "q" (swap! app-state assoc :message "Quit? Just close the tab") (swap! app-state assoc :message (str "Unknown cmd: :" key)))
              (when (or (= key "Enter") (= key "w") (= key "q")) (swap! app-state assoc :mode "normal"))
              (render))
          ;; VISUAL MODE
          (if (= mode "visual")
            (let [vtype (:visual-type state)]
              (case key
                "h" (swap! app-state cmd-move-cursor 0 -1)
                "j" (swap! app-state cmd-move-cursor 1 0)
                "k" (swap! app-state cmd-move-cursor -1 0)
                "l" (swap! app-state cmd-move-cursor 0 1)
                "0" (swap! app-state assoc-in [:cursor :col] 0)
                "$" (swap! app-state (fn [s] (let [lines (get-lines s) row (:row (:cursor s))] (assoc-in s [:cursor :col] (.-length (aget lines row))))))
                "w" (swap! app-state cmd-move-word-forward)
                "b" (swap! app-state cmd-move-word-backward)
                "d" (swap! app-state cmd-delete-selection)
                "x" (swap! app-state cmd-delete-selection)
                "y" (swap! app-state cmd-yank-selection)
                (swap! app-state assoc :message (str "Unknown: " key)))
              (render))
            ;; Ctrl+V in normal mode -> enter visual block mode
            (if (and ctrl (= key "v"))
              (do (swap! app-state cmd-enter-visual "block") (render))
              ;; NORMAL MODE - simple flat dispatch
              (let [op (:operator state)
                tot (:text-object-type state)]
            (cond
              ;; text-object mode
              tot
              (let [obj-keys #{"w" "\"" "'" "`"}]
                (if (contains? obj-keys key)
                  (let [bounds (text-object-bounds (:text state) (:cursor state) tot key)]
                    (swap! app-state apply-operator-to-bounds op bounds))
                  (swap! app-state assoc :operator nil :text-object-type nil))
                (render))
              ;; operator-pending
              op
              (cond
                ;; text-object prefix: i/a
                (or (= key "i") (= key "a"))
                (do (swap! app-state assoc :text-object-type (if (= key "i") "inner" "around")) (render))
                ;; motion keys
                (motion-key? key)
                (do (let [motion-s (apply-motion state key)] (swap! app-state apply-operator op motion-s)) (render))
                ;; find/till motions
                (contains? #{"f" "F" "t" "T"} key)
                (do (reset! pending-find {:type key :dir (case key ("f" "t") :forward ("F" "T") :backward) :operator op})
                    (swap! app-state assoc :operator nil)
                    (render))
                ;; delete-surround: ds (delete then s)
                (and (= op "delete") (= key "s"))
                (do (swap! app-state assoc :operator "delete-surround") (render))
                ;; delete-surround target key
                (= op "delete-surround")
                (do (swap! app-state (fn [s] (cmd-delete-surround s key))) (swap! app-state assoc :operator nil) (render))
                ;; dd: delete line
                (and (= op "delete") (= key "d"))
                (do (swap! app-state cmd-delete-line) (render))
                ;; cc: change line
                (and (= op "change") (= key "c"))
                (do (swap! app-state cmd-delete-line) (swap! app-state assoc :mode "insert") (render))
                ;; yy: yank line
                (and (= op "yank") (= key "y"))
                (do (swap! app-state cmd-yank-line) (render))
                ;; else: cancel operator
                :else
                (do (swap! app-state assoc :operator nil :message "") (render)))
              ;; pending-find
              @pending-find
              (let [fi @pending-find char key pre-state @app-state]
                (reset! pending-find nil)
                (case (:type fi)
                  "f" (do (swap! app-state cmd-find-char-forward char) (reset! last-find {:char char :type "f" :dir :forward}))
                  "F" (do (swap! app-state cmd-find-char-backward char) (reset! last-find {:char char :type "F" :dir :backward}))
                  "t" (do (swap! app-state cmd-find-till-forward char) (reset! last-find {:char char :type "t" :dir :forward}))
                  "T" (do (swap! app-state cmd-find-till-backward char) (reset! last-find {:char char :type "T" :dir :backward})))
                (if (:operator fi)
                  (let [post-state @app-state] (reset! app-state (apply-operator pre-state (:operator fi) post-state)) (render))
                  (render)))
              ;; normal keys
              :else
              (do (case key
                    "h" (swap! app-state cmd-move-cursor 0 -1)
                    "j" (swap! app-state cmd-move-cursor 1 0)
                    "k" (swap! app-state cmd-move-cursor -1 0)
                    "l" (swap! app-state cmd-move-cursor 0 1)
                    "i" (swap! app-state assoc :mode "insert")
                    "o" (swap! app-state cmd-open-below)
                    "O" (swap! app-state cmd-open-above)
                    "a" (swap! app-state cmd-insert-after)
                    "I" (swap! app-state #(-> % (assoc :mode "insert") (assoc-in [:cursor :col] 0)))
                    "A" (swap! app-state (fn [s] (let [lines (get-lines s) row (:row (:cursor s))] (-> s (assoc :mode "insert") (assoc-in [:cursor :col] (.-length (aget lines row)))))))
                    "v" (swap! app-state cmd-enter-visual "char")
                    "V" (swap! app-state cmd-enter-visual "line")
                    "x" (swap! app-state cmd-delete-char)
                    "d" (let [now (js/Date.now) time-diff (- now @last-key-time)]
                          (reset! last-key-time now)
                          (if (and @pending-d (< time-diff 500))
                            (do (reset! pending-d false) (swap! app-state cmd-delete-line))
                            (do (reset! pending-d true) (swap! app-state assoc :operator "delete") (js/setTimeout (fn [] (reset! pending-d false) (swap! app-state assoc :operator nil)) 500))))
                    "D" (swap! app-state cmd-delete-to-end-of-line)
                    "Y" (swap! app-state cmd-yank-to-end-of-line)
                    "p" (swap! app-state cmd-paste-after)
                    "P" (swap! app-state cmd-paste-before)
                    "c" (swap! app-state assoc :operator "change")
                    "y" (swap! app-state assoc :operator "yank")
                    "u" (swap! app-state cmd-undo)
                    "0" (swap! app-state assoc-in [:cursor :col] 0)
                    "$" (swap! app-state (fn [s] (let [lines (get-lines s) row (:row (:cursor s))] (assoc-in s [:cursor :col] (.-length (aget lines row))))))
                    "w" (swap! app-state cmd-move-word-forward)
                    "b" (swap! app-state cmd-move-word-backward)
                    "g" (let [now (js/Date.now) time-diff (- now @last-key-time)]
                          (reset! last-key-time now)
                          (if (and @pending-g (< time-diff 500))
                            (do (reset! pending-g false) (swap! app-state assoc-in [:cursor :row] 0) (swap! app-state assoc-in [:cursor :col] 0))
                            (do (reset! pending-g true) (js/setTimeout (fn [] (reset! pending-g false)) 500))))
                    "G" (swap! app-state (fn [s] (let [lines (get-lines s)] (-> s (assoc-in [:cursor :row] (dec (.-length lines))) (assoc-in [:cursor :col] 0)))))
                    "f" (reset! pending-find {:type "f" :dir :forward})
                    "F" (reset! pending-find {:type "F" :dir :backward})
                    "t" (reset! pending-find {:type "t" :dir :forward})
                    "T" (reset! pending-find {:type "T" :dir :backward})
                    ";" (if-let [lf @last-find]
                          (let [ch (:char lf) tp (:type lf) dr (:dir lf)]
                            (case tp
                              "f" (if (= dr :forward) (swap! app-state cmd-find-char-forward ch) (swap! app-state cmd-find-char-backward ch))
                              "F" (if (= dr :backward) (swap! app-state cmd-find-char-backward ch) (swap! app-state cmd-find-char-forward ch))
                              "t" (if (= dr :forward) (swap! app-state cmd-find-till-forward ch) (swap! app-state cmd-find-till-backward ch))
                              "T" (if (= dr :backward) (swap! app-state cmd-find-till-backward ch) (swap! app-state cmd-find-till-forward ch))))
                          (swap! app-state assoc :message "No previous find"))
                    "," (if-let [lf @last-find]
                          (let [ch (:char lf) tp (:type lf) dr (:dir lf) opp (if (= dr :forward) :backward :forward)]
                            (case tp
                              "f" (if (= opp :forward) (swap! app-state cmd-find-char-forward ch) (swap! app-state cmd-find-char-backward ch))
                              "F" (if (= opp :backward) (swap! app-state cmd-find-char-backward ch) (swap! app-state cmd-find-char-forward ch))
                              "t" (if (= opp :forward) (swap! app-state cmd-find-till-forward ch) (swap! app-state cmd-find-till-backward ch))
                              "T" (if (= opp :backward) (swap! app-state cmd-find-till-backward ch) (swap! app-state cmd-find-till-forward ch))))
                          (swap! app-state assoc :message "No previous find"))
                    ":" (swap! app-state assoc :mode "command")
                    (swap! app-state assoc :message (str "Unknown: " key)))
                  (render)))))))))))

;; ============================================================
;; UI Rendering
;; ============================================================
(defn render []
  (let [state @app-state
        lines (get-lines state)
        mode (:mode state)
        cursor (:cursor state)
        msg (:message state)
        cursor-row (:row cursor)
        cursor-col (:col cursor)
        vtype (:visual-type state)
        vstart (:visual-start state)
        visual-mode? (and (= mode "visual") vstart)]
    (set! (.-innerHTML (js/document.getElementById "app"))
      (str "<div class='vim-container'>"
           "<div class='vim-left'>"
           "<div class='vim-header'>EDITOR - "
           (.toUpperCase mode) " MODE</div>"
           "<div class='vim-editor' id='vim-editor' contenteditable='false'>"
           (.join (.map lines (fn [line i]
               (if visual-mode?
                 (case vtype
                   "block"
                   (let [sr (min (:row vstart) cursor-row)
                         er (max (:row vstart) cursor-row)
                         line-len (.-length line)
                         sc (min (min (:col vstart) cursor-col) line-len)
                         ec (min (inc (max (:col vstart) cursor-col)) line-len)]
                     (if (<= sr i er)
                       (str "<div class='vim-line"
                            (if (= i cursor-row) " vim-current-line" "")
                            "'>"
                            (escape-html (.slice line 0 sc))
                            "<span class='vim-visual-block'>"
                            (escape-html (.slice line sc ec))
                            "</span>"
                            (escape-html (.slice line ec))
                            "</div>")
                       (if (= i cursor-row)
                         (let [before (escape-html (.slice line 0 cursor-col))
                               at-char (if (< cursor-col (.-length line))
                                         (escape-html (.slice line cursor-col (inc cursor-col)))
                                         " ")]
                           (str "<div class='vim-line vim-current-line'>"
                                before
                                "<span class='vim-cursor-line'>|</span>"
                                (escape-html (.slice line (inc cursor-col)))
                                "</div>"))
                         (str "<div class='vim-line'>" (escape-html line) "</div>"))))
                   "line"
                   (let [bounds (visual-bounds state)
                         sr (:row (:start bounds))
                         er (:row (:end bounds))]
                     (if (<= sr i er)
                       (str "<div class='vim-line"
                            (if (= i cursor-row) " vim-current-line" "")
                            "'><span class='vim-visual-block'>"
                            (escape-html line)
                            "</span></div>")
                       (str "<div class='vim-line'>"
                            (escape-html line)
                            "</div>")))
                   ;; "char" (default)
                   (let [bounds (visual-bounds state)
                         sr (:row (:start bounds))
                         sc (:col (:start bounds))
                         er (:row (:end bounds))
                         ec (:col (:end bounds))]
                     (cond
                       (< i sr)
                       (str "<div class='vim-line'>" (escape-html line) "</div>")
                       (> i er)
                       (str "<div class='vim-line'>" (escape-html line) "</div>")
                       (= i sr)
                       (if (= sr er)
                         (let [line-len (.-length line)
                               sc2 (min sc line-len)
                               ec2 (min (inc ec) line-len)]
                           (str "<div class='vim-line"
                                (if (= i cursor-row) " vim-current-line" "")
                                "'>"
                                (escape-html (.slice line 0 sc2))
                                "<span class='vim-visual-block'>"
                                (escape-html (.slice line sc2 ec2))
                                "</span>"
                                (escape-html (.slice line ec2))
                                "</div>"))
                         (let [line-len (.-length line)
                               sc2 (min sc line-len)]
                           (str "<div class='vim-line"
                                (if (= i cursor-row) " vim-current-line" "")
                                "'>"
                                (escape-html (.slice line 0 sc2))
                                "<span class='vim-visual-block'>"
                                (escape-html (.slice line sc2))
                                "</span></div>")))
                       (= i er)
                       (let [ec2 (min (inc ec) (.-length line))]
                         (str "<div class='vim-line"
                              (if (= i cursor-row) " vim-current-line" "")
                              "'>"
                              "<span class='vim-visual-block'>"
                              (escape-html (.slice line 0 ec2))
                              "</span>"
                              (escape-html (.slice line ec2))
                              "</div>"))
                       :else
                       (str "<div class='vim-line"
                            (if (= i cursor-row) " vim-current-line" "")
                            "'><span class='vim-visual-block'>"
                            (escape-html line)
                            "</span></div>"))))
                 ;; Non-visual mode: normal display
                 (if (= i cursor-row)
                   (let [before (escape-html (.slice line 0 cursor-col))
                         at-char (if (< cursor-col (.-length line))
                                   (escape-html (.slice line cursor-col (inc cursor-col)))
                                   " ")]
                     (str "<div class='vim-line vim-current-line'>"
                          before
                          (if (= mode "normal")
                            (str "<span class='vim-cursor-block'>" at-char "</span>")
                            (str "<span class='vim-cursor-line'>|</span>"))
                          (escape-html (.slice line (inc cursor-col)))
                          "</div>"))
                   (str "<div class='vim-line'>" (escape-html line) "</div>"))))) "")
           "</div>"
           "<div class='vim-statusbar'>"
           "<span class='vim-mode-" mode "'>" (.toUpperCase mode) "</span>"
           (if (not= msg "") (str " <span class='vim-msg'>" msg "</span>") "")
           " | row: " (inc cursor-row) " col: " (inc cursor-col)
           " | " (.-length lines) " lines"
           "</div>"
           "</div>"
           "<div class='vim-right'>"
           "<div class='vim-header'>ORG PREVIEW</div>"
           "<div class='org-preview' id='org-preview'>"
           (org-to-html (:text state))
           "</div>"
           "</div>"
           "</div>"))))

;; ============================================================
;; CSS styles
;; ============================================================


(defn init []
  (js/document.addEventListener "keydown" handle-key)
  (render)
  (println "Vim Org Editor initialized!"))

(init)


(defn block-bounds [s] (let [m (:mode s) vt (:visual-type s) vs (:visual-start s) c (:cursor s) cr (:row c) cc (:col c)] (if (and (= m "visual") (= vt "block") vs) {:start-row (min (:row vs) cr) :start-col (min (:col vs) cc) :end-row (max (:row vs) cr) :end-col (max (:col vs) cc)} nil)))
