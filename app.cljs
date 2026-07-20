(require '["https://esm.sh/orga" :as orga])
(require '["https://unpkg.com/squint-cljs/src/squint/string.js" :as str])

;; ============================================================
;; Utility: escape HTML
;; ============================================================
(defn escape-html [s]
  (if s
    (-> (str s)
        (.replaceAll "&" "&amp;")
        (.replaceAll "<" "&lt;")
        (.replaceAll ">" "&gt;")
        (.replaceAll "\"" "&quot;"))
    ""))

;; ============================================================
;; Org-mode AST to HTML converter
;; ============================================================
(defn node->html [node]
  (let [t (.-type node)]
    (case t
      "document" (let [children (or (.-children node) #js [])]
                   (str "<div class='org-doc'>"
                        (.join (.map children (fn [c] (node->html c))) "")
                        "</div>"))
      "section" (let [children (or (.-children node) #js [])]
                  (str "<div class='org-section org-level-" (or (.-level node) "") "'>"
                       (.join (.map children (fn [c] (node->html c))) "")
                       "</div>"))

      ;; --- BULLETPROOF COLORED HEADINGS ---
      "headline" (let [children (or (.-children node) #js [])
                       ;; Fallback: if orga fails to set 'level', find the stars node and count them
                       stars-node (.find children (fn [c] (= "stars" (.-type c))))
                       stars-val (if stars-node (or (.-value stars-node) "") "")
                       stars-len (.-length stars-val)
                       level (js/Number (or (.-level node) (if (> stars-len 0) stars-len 1)))
                       ;; Modulo math that seamlessly wraps back around past level 6
                       lvl-mod (mod level 6)
                       color (cond
                               (= lvl-mod 1) "#005cc5"  ;; Level 1: Blue
                               (= lvl-mod 2) "#22863a"  ;; Level 2: Green
                               (= lvl-mod 3) "#6f42c1"  ;; Level 3: Purple
                               (= lvl-mod 4) "#d73a49"  ;; Level 4: Red
                               (= lvl-mod 5) "#e36209"  ;; Level 5: Orange
                               :else "#b08800")         ;; Level 6: Gold
                       filtered (.filter children (fn [c] (not= "stars" (.-type c))))]
                   (str "<h" level " class='org-headline' style='color: " color " !important; margin-top: 0.5em;'>"
                        (.join (.map filtered (fn [c] (node->html c))) "")
                        "</h" level ">"))

      "paragraph" (let [children (or (.-children node) #js [])]
                    (str "<p>"
                         (.join (.map children (fn [c] (node->html c))) "")
                         "</p>"))
      "text" (let [style (.-style node)
                   val (or (.-value node) "")]
               (cond (= style "italic") (str "<em>" (escape-html val) "</em>")
                     (= style "bold") (str "<strong>" (escape-html val) "</strong>")
                     (= style "code") (str "<code>" (escape-html val) "</code>")
                     (= style "verbatim") (str "<code>" (escape-html val) "</code>")
                     :else (escape-html val)))
      "list" (let [children (or (.-children node) #js [])]
               (str "<ul>"
                    (.join (.map children (fn [c] (node->html c))) "")
                    "</ul>"))
      "list.item" (let [children (or (.-children node) #js [])
                        f1 (.filter children (fn [c] (not= "list.item.bullet" (.-type c))))
                        f2 (.filter f1 (fn [c] (not= "newline" (.-type c))))]
                    (str "<li>"
                         (.join (.map f2 (fn [c] (node->html c))) "")
                         "</li>"))

      "block" (let [name (or (.-name node) "")
                    params (.-params node)
                    lang (cond
                           (nil? params) ""
                           (= (js/typeof params) "string") params
                           (and (.-length params) (> (.-length params) 0)) (aget params 0)
                           :else "")
                    val (if (.-value node)
                          (.-value node)
                          (if (.-children node)
                            (.join (.map (.-children node) (fn [c] (or (.-value c) ""))) "")
                            ""))]
                (if (= (.toLowerCase name) "src")
                  (str "<pre style='background: #f4f4f4; padding: 10px; border-radius: 5px;'><code class='language-" lang "'>"
                       (escape-html val)
                       "</code></pre>")
                  (str "<pre class='org-block'>" (escape-html val) "</pre>")))

      "keyword" ""
      "emptyLine" ""
      "stars" ""
      "newline" ""
      "list.item.bullet" ""

      (str "<div style='color: red; border: 1px solid red; margin: 2px;'>[Unhandled AST node: <b>" t "</b>]</div>"))))

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
;; ============================================================
;; Vim Editor State
;; ============================================================
(def storage-key "vim-org-editor-content")
(def default-text "Hello from Vim!\nThis is the editor.\n\n* Org mode heading\nSome text here")

(def app-state
  (atom {:text (or (.getItem js/localStorage storage-key) default-text)
         :mode "normal"
         :cursor {:row 0 :col 0}
         :visual-start nil
         :visual-type nil
         :history []
         :message ""
         :operator nil
         :text-object-type nil
         :surround-old nil
         :surround-bounds nil
         :command-text ""
         :last-search nil}))


(defn get-lines [s]
  (.split (:text s) "\n"))

(defn clamp [v mn mx]
  (max mn (min v mx)))

(defn line-length [lines row]
  (if (and lines (< row (.-length lines)))
    (.-length (aget lines row))
    0))

(defn save-history [s]
  (assoc s :history (conj (or (:history s) []) (:text s))))

;; ============================================================
;; Word Motion Engine (w, b, e, ge)
;; ============================================================
(defn text-pos->index [text row col]
  (let [lines (.split text "\n")]
    (loop [r 0 idx 0]
      (if (< r row)
        (recur (inc r) (+ idx (.-length (aget lines r)) 1))
        (+ idx col)))))

(defn index->text-pos [text idx]
  (let [lines (.split text "\n")]
    (loop [r 0 curr 0]
      (if (>= r (.-length lines))
        (let [last-row (max 0 (dec (.-length lines)))]
          {:row last-row :col (.-length (aget lines last-row))})
        (let [len (.-length (aget lines r))]
          (if (< idx (+ curr len 1))
            (let [col (min (- idx curr) len)]
              {:row r :col col})
            (recur (inc r) (+ curr len 1))))))))

(defn c-class [text idx]
  (if (or (< idx 0) (>= idx (.-length text)))
    :eof
    (let [c (.slice text idx (inc idx))]
      (cond
        (= c "\n") :space
        (re-find #"[ \t\r]" c) :space
        (re-find #"[a-zA-Z0-9_]" c) :word
        :else :punct))))

(defn cmd-move-word-forward [s]
  (let [text (:text s)
        start-idx (text-pos->index text (:row (:cursor s)) (:col (:cursor s)))
        len (.-length text)
        orig-cls (c-class text start-idx)]
    (let [next-idx
          (loop [i (inc start-idx) phase :skip-word]
            (if (>= i len)
              len
              (let [cls (c-class text i)]
                (case phase
                  :skip-word
                  (if (= cls orig-cls)
                    (recur (inc i) :skip-word)
                    (if (= cls :space)
                      (recur (inc i) :skip-space)
                      i))
                  :skip-space
                  (if (= cls :space)
                    (recur (inc i) :skip-space)
                    i)))))]
      (assoc s :cursor (index->text-pos text next-idx)))))

(defn cmd-move-word-backward [s]
  (let [text (:text s)
        start-idx (text-pos->index text (:row (:cursor s)) (:col (:cursor s)))]
    (let [next-idx
          (loop [i (dec start-idx) phase :skip-space target-cls nil]
            (if (< i 0)
              0
              (let [cls (c-class text i)]
                (case phase
                  :skip-space
                  (if (= cls :space)
                    (recur (dec i) :skip-space nil)
                    (recur (dec i) :skip-word cls))
                  :skip-word
                  (if (= cls target-cls)
                    (recur (dec i) :skip-word target-cls)
                    (inc i))))))]
      (assoc s :cursor (index->text-pos text next-idx)))))

(defn cmd-move-word-end-forward [s]
  (let [text (:text s)
        start-idx (text-pos->index text (:row (:cursor s)) (:col (:cursor s)))
        len (.-length text)]
    (let [next-idx
          (loop [i (inc start-idx) phase :skip-space target-cls nil]
            (if (>= i len)
              (max 0 (dec len))
              (let [cls (c-class text i)]
                (case phase
                  :skip-space
                  (if (= cls :space)
                    (recur (inc i) :skip-space nil)
                    (recur (inc i) :skip-word cls))
                  :skip-word
                  (if (= cls target-cls)
                    (recur (inc i) :skip-word target-cls)
                    (dec i))))))]
      (assoc s :cursor (index->text-pos text next-idx)))))

(defn cmd-move-word-end-backward [s]
  (let [text (:text s)
        start-idx (text-pos->index text (:row (:cursor s)) (:col (:cursor s)))
        orig-cls (c-class text start-idx)]
    (let [next-idx
          (loop [i (dec start-idx) phase :skip-word]
            (if (< i 0)
              0
              (let [cls (c-class text i)]
                (case phase
                  :skip-word
                  (if (= cls orig-cls)
                    (recur (dec i) :skip-word)
                    (if (= cls :space)
                      (recur (dec i) :skip-space)
                      i))
                  :skip-space
                  (if (= cls :space)
                    (recur (dec i) :skip-space)
                    i)))))]
      (assoc s :cursor (index->text-pos text next-idx)))))

;; ============================================================
;; Vim Commands (pure functions returning new state)
;; ============================================================
(defn cmd-execute-search [s query dir]
  (if (or (nil? query) (= query ""))
    (assoc s :message "No search pattern")
    (let [text (:text s)
          start-idx (text-pos->index text (:row (:cursor s)) (:col (:cursor s)))
          match-idx (if (= dir :forward)
                      (let [idx (.indexOf text query (inc start-idx))]
                        (if (>= idx 0) idx (.indexOf text query 0)))
                      (let [sub (.slice text 0 start-idx)
                            idx (.lastIndexOf sub query)]
                        (if (>= idx 0) idx (.lastIndexOf text query))))]
      (if (and match-idx (>= match-idx 0))
        (-> s
            (assoc :cursor (index->text-pos text match-idx))
            (assoc :last-search query)
            (assoc :message (if (and (= dir :forward) (< match-idx start-idx)) "search hit BOTTOM, continuing at TOP"
                                (if (and (= dir :backward) (> match-idx start-idx)) "search hit TOP, continuing at BOTTOM"
                                    (str "/" query)))))
        (assoc s :message (str "Pattern not found: " query) :last-search query)))))

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
      (-> s
          (save-history)
          (assoc :text (replace-line lines row
                        (str (.slice line 0 (dec col)) (.slice line col))))
          (assoc-in [:cursor :col] (dec col)))
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
  (let [lines (get-lines s)
        row (:row (:cursor s))
        line (aget lines row)]
    (assoc s :clipboard line :message "Yanked")))

(defn cmd-yank-to-end-of-line [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        col (:col (:cursor s))
        line (aget lines row)]
    (if (< col (.-length line))
      (assoc s :clipboard (.slice line col) :message "Yanked")
      (assoc s :message ""))))

(defn cmd-paste-after [s]
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
;; Visual Mode & Range operations
;; ============================================================
(defn cmd-enter-visual [s type]
  (assoc s :mode "visual" :visual-type type :visual-start (:cursor s)))

(defn visual-bounds [s]
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
            {:start {:row er :col 0} :end {:row sr :col last-line-len}}
            {:start {:row sr :col 0} :end {:row er :col last-line-len}}))
        "block"
        (let [sr (:row vstart) sc (:col vstart)
              er (:row cursor) ec (:col cursor)]
          {:start {:row (min sr er) :col (min sc ec)}
           :end {:row (max sr er) :col (max sc ec)}})
        (let [sr (:row vstart) sc (:col vstart)
              er (:row cursor) ec (:col cursor)]
          (if (or (< er sr) (and (= er sr) (< ec sc)))
            {:start {:row er :col ec} :end {:row sr :col sc}}
            {:start {:row sr :col sc} :end {:row er :col ec}}))))))

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

(defn delete-block-range [text start-row start-col end-row end-col]
  (let [lines (.split text "\n")
        new-lines (.map lines (fn [line row]
                                (if (and (>= row start-row) (<= row end-row))
                                  (if (>= start-col (.-length line))
                                    line
                                    (str (.slice line 0 start-col)
                                         (.slice line (inc end-col))))
                                  line)))]
    (.join new-lines "\n")))

(defn get-selected-text [s]
  (let [bounds (visual-bounds s)
        start (:start bounds) end (:end bounds)
        sr (:row start) sc (:col start)
        er (:row end) ec (:col end)
        vtype (:visual-type s)
        lines (.split (:text s) "\n")]
    (case vtype
      "block"
      (let [rows (vec (range sr (inc er)))
            selected-lines (.map rows (fn [row]
                                        (let [line (aget lines row)]
                                          (.slice line sc (inc ec)))))]
        (.join selected-lines "\n"))
      (get-text-range (:text s) sr sc er ec))))

(defn cmd-delete-selection [s]
  (let [bounds (visual-bounds s)
        start (:start bounds) end (:end bounds)
        sr (:row start) sc (:col start)
        er (:row end) ec (:col end)
        vtype (:visual-type s)]
    (case vtype
      "block"
      (let [new-text (delete-block-range (:text s) sr sc er ec)]
        (-> s save-history (assoc :text new-text :mode "normal" :visual-start nil :visual-type nil :cursor {:row sr :col sc})))
      (let [new-text (delete-range (:text s) sr sc er ec)]
        (-> s save-history (assoc :text new-text :mode "normal" :visual-start nil :visual-type nil :cursor {:row sr :col sc}))))))

(defn cmd-yank-selection [s]
  (let [yanked-text (get-selected-text s)]
    (-> s (assoc :clipboard yanked-text :mode "normal" :visual-start nil :visual-type nil :message "Yanked"))))

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
        new-lines (.concat (.slice lines 0 (inc row)) #js [""] (.slice lines (inc row)))]
    (-> s save-history (assoc :text (.join new-lines "\n") :cursor {:row (inc row) :col 0} :mode "insert"))))

(defn cmd-open-above [s]
  (let [lines (get-lines s)
        row (:row (:cursor s))
        new-lines (.concat (.slice lines 0 row) #js [""] (.slice lines row))]
    (-> s save-history (assoc :text (.join new-lines "\n") :cursor {:row row :col 0} :mode "insert"))))

(defn cmd-find-char-forward [s char]
  (let [lines (get-lines s)
        row (:row (:cursor s)) col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (inc col) found (.indexOf line char start-pos)]
        (if (>= found 0) (assoc-in s [:cursor :col] found) s))
      s)))

(defn cmd-find-char-backward [s char]
  (let [lines (get-lines s)
        row (:row (:cursor s)) col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (dec col) found (.lastIndexOf line char start-pos)]
        (if (>= found 0) (assoc-in s [:cursor :col] found) s))
      s)))

(defn cmd-find-till-forward [s char]
  (let [lines (get-lines s)
        row (:row (:cursor s)) col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (inc col) found (.indexOf line char start-pos)]
        (if (> found 0) (assoc-in s [:cursor :col] (dec found)) s))
      s)))

(defn cmd-find-till-backward [s char]
  (let [lines (get-lines s)
        row (:row (:cursor s)) col (:col (:cursor s))
        line (aget lines row)]
    (if line
      (let [start-pos (dec col) found (.lastIndexOf line char start-pos)]
        (if (>= found 0)
          (let [new-col (inc found)] (if (<= new-col col) (assoc-in s [:cursor :col] new-col) s))
          s))
      s)))

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
    "e" (cmd-move-word-end-forward s)
    "0" (assoc-in s [:cursor :col] 0)
    "$" (let [lines (.split (:text s) "\n") row (:row (:cursor s))]
          (assoc-in s [:cursor :col] (.-length (aget lines row))))
    s))

;; ============================================================
;; Surround operations (vim-surround)
;; ============================================================
(def surround-pairs {"(" ")" ")" "(" "[" "]" "]" "[" "{" "}" "}" "{" "\"" "\"" "'" "'" "`" "`" "<" ">" ">" "<"})

(defn get-surround-pair [char]
  (let [openers #{"(" "[" "{" "<"}
        closers #{")" "]" "}" ">"}]
    (cond
      (contains? openers char) [char (get surround-pairs char char)]
      (contains? closers char) [(get surround-pairs char char) char]
      :else [char char])))

(defn cmd-delete-surround [s char]
  (let [lines (.split (:text s) "\n")
        row (:row (:cursor s)) col (:col (:cursor s))
        line (aget lines row)
        openers #{"(" "[" "{" "<"} closers #{")" "]" "}" ">"} quotes #{"\"" "'" "`"}]
    (if (contains? quotes char)
      (let [open-pos (loop [c col] (if (< c 0) nil (if (= (.slice line c (inc c)) char) c (recur (dec c)))))
            close-pos (when open-pos (loop [c (inc open-pos)] (if (>= c (.-length line)) nil (if (= (.slice line c (inc c)) char) c (recur (inc c))))))]
        (if (and open-pos close-pos)
          (let [new-line (str (.slice line 0 open-pos) (.slice line (inc open-pos) close-pos) (.slice line (inc close-pos)))
                new-text (replace-line lines row new-line)]
            (assoc s :text new-text :cursor {:row row :col (max 0 (dec col))}))
          (assoc s :message "No matching surround found")))
      (let [[opener closer] (if (contains? openers char)
                              [char (get surround-pairs char)]
                              [(get surround-pairs char) char])
            open-pos (loop [c (dec col)] (if (< c 0) nil (if (= (.slice line c (inc c)) opener) c (recur (dec c)))))]
        (if open-pos
          (let [close-pos (loop [i (inc open-pos) depth 1]
                            (if (>= i (.-length line)) nil
                                (let [c (.slice line i (inc i))]
                                  (cond (= c opener) (recur (inc i) (inc depth))
                                        (= c closer) (if (= depth 1) i (recur (inc i) (dec depth)))
                                        :else (recur (inc i) depth)))))]
            (if close-pos
              (let [new-line (str (.slice line 0 open-pos) (.slice line (inc open-pos) close-pos) (.slice line (inc close-pos)))
                    new-text (replace-line lines row new-line)]
                (assoc s :text new-text :cursor {:row row :col (max 0 (dec col))}))
              (assoc s :message "No matching surround found")))
          (let [fwd-open (loop [c col] (if (>= c (.-length line)) nil (if (= (.slice line c (inc c)) opener) c (recur (inc c)))))]
            (if fwd-open
              (let [close-pos (loop [i (inc fwd-open) depth 1]
                                (if (>= i (.-length line)) nil
                                    (let [c (.slice line i (inc i))]
                                      (cond (= c opener) (recur (inc i) (inc depth))
                                            (= c closer) (if (= depth 1) i (recur (inc i) (dec depth)))
                                            :else (recur (inc i) depth)))))]
                (if close-pos
                  (let [new-line (str (.slice line 0 fwd-open) (.slice line (inc fwd-open) close-pos) (.slice line (inc close-pos)))
                        new-text (replace-line lines row new-line)]
                    (assoc s :text new-text :cursor {:row row :col (max 0 (min col (dec col)))}))
                  (assoc s :message "No matching surround found")))
              (assoc s :message "No matching surround found"))))))))

(defn cmd-change-surround [s old-char new-char]
  (let [lines (.split (:text s) "\n")
        row (:row (:cursor s)) col (:col (:cursor s))
        line (aget lines row)
        openers #{"(" "[" "{" "<"} closers #{")" "]" "}" ">"} quotes #{"\"" "'" "`"}
        [new-open new-close] (get-surround-pair new-char)]
    (if (contains? quotes old-char)
      (let [open-pos (loop [c col] (if (< c 0) nil (if (= (.slice line c (inc c)) old-char) c (recur (dec c)))))
            close-pos (when open-pos (loop [c (inc open-pos)] (if (>= c (.-length line)) nil (if (= (.slice line c (inc c)) old-char) c (recur (inc c))))))]
        (if (and open-pos close-pos)
          (let [new-line (str (.slice line 0 open-pos) new-open (.slice line (inc open-pos) close-pos) new-close (.slice line (inc close-pos)))
                new-text (replace-line lines row new-line)]
            (-> s save-history (assoc :text new-text :cursor {:row row :col open-pos})))
          (assoc s :message "No matching surround found")))
      (let [[opener closer] (if (contains? openers old-char) [old-char (get surround-pairs old-char)] [(get surround-pairs old-char) old-char])
            open-pos (loop [c (dec col)] (if (< c 0) nil (if (= (.slice line c (inc c)) opener) c (recur (dec c)))))]
        (if open-pos
          (let [close-pos (loop [i (inc open-pos) depth 1]
                            (if (>= i (.-length line)) nil
                                (let [c (.slice line i (inc i))]
                                  (cond (= c opener) (recur (inc i) (inc depth))
                                        (= c closer) (if (= depth 1) i (recur (inc i) (dec depth)))
                                        :else (recur (inc i) depth)))))]
            (if close-pos
              (let [new-line (str (.slice line 0 open-pos) new-open (.slice line (inc open-pos) close-pos) new-close (.slice line (inc close-pos)))
                    new-text (replace-line lines row new-line)]
                (-> s save-history (assoc :text new-text :cursor {:row row :col open-pos})))
              (assoc s :message "No matching surround found")))
          (assoc s :message "No matching surround found"))))))

(defn cmd-apply-add-surround [s char]
  (let [bounds (:surround-bounds s)
        sr (:row (:start bounds)) sc (:col (:start bounds))
        er (:row (:end bounds)) ec (:col (:end bounds))
        lines (.split (:text s) "\n")
        [new-open new-close] (get-surround-pair char)]
    (if (= sr er)
      (let [line (aget lines sr)
            new-line (str (.slice line 0 sc) new-open (.slice line sc ec) new-close (.slice line ec))]
        (-> s save-history (assoc :text (replace-line lines sr new-line) :operator nil :surround-bounds nil)))
      (let [first-line (aget lines sr) last-line (aget lines er)
            new-first (str (.slice first-line 0 sc) new-open (.slice first-line sc))
            new-last (str (.slice last-line 0 ec) new-close (.slice last-line ec))
            new-lines (.concat (.slice lines 0 sr) #js [new-first] (.slice lines (inc sr) er) #js [new-last] (.slice lines (inc er)))]
        (-> s save-history (assoc :text (.join new-lines "\n") :operator nil :surround-bounds nil))))))

(defn apply-operator-to-bounds [s operator bounds]
  (let [text (:text s)
        start (:start bounds) end (:end bounds)
        start-row (:row start) start-col (:col start)
        end-row (:row end) end-col (:col end)]
    (case operator
      "delete" (let [new-text (delete-range text start-row start-col end-row end-col)]
                 (-> s save-history (assoc :text new-text :cursor {:row start-row :col start-col} :operator nil :text-object-type nil)))
      "yank" (let [yanked-text (get-text-range text start-row start-col end-row end-col)]
               (assoc s :clipboard yanked-text :operator nil :message "Yanked" :text-object-type nil))
      "change" (let [new-text (delete-range text start-row start-col end-row end-col)]
                 (-> s save-history (assoc :text new-text :cursor {:row start-row :col start-col} :mode "insert" :operator nil :text-object-type nil)))
      "add-surround-motion" (assoc s :operator "add-surround-char" :surround-bounds bounds :text-object-type nil)
      (assoc s :operator nil :text-object-type nil))))

(defn apply-operator [s operator motion-s]
  (let [text (:text s) cursor (:cursor s)
        start-row (:row cursor) start-col (:col cursor)
        end-cursor (:cursor motion-s)
        end-row (:row end-cursor) end-col (:col end-cursor)
        [sr sc er ec] (if (or (< end-row start-row) (and (= end-row start-row) (< end-col start-col)))
                        [end-row end-col start-row start-col]
                        [start-row start-col end-row end-col])]
    (case operator
      "delete" (let [new-text (delete-range text sr sc er ec)]
                 (-> s save-history (assoc :text new-text :cursor {:row sr :col sc} :operator nil)))
      "yank" (let [yanked-text (get-text-range text sr sc er ec)]
               (assoc s :clipboard yanked-text :operator nil :message "Yanked"))
      "change" (let [new-text (delete-range text sr sc er ec)]
                 (-> s save-history (assoc :text new-text :cursor {:row sr :col sc} :mode "insert" :operator nil)))
      "add-surround-motion" (assoc s :operator "add-surround-char" :surround-bounds {:start {:row sr :col sc} :end {:row er :col ec}})
      (assoc s :operator nil))))

;; ============================================================
;; Text Object Functions
;; ============================================================
(defn word-bounds [text cursor]
  (let [lines (.split text "\n")
        row (:row cursor) col (:col cursor)
        line (aget lines row)]
    (if (or (nil? line) (>= col (.-length line)))
      (let [len (.-length line)] {:start {:row row :col len} :end {:row row :col len}})
      (let [re #"[a-zA-Z0-9_]"
            ch (.slice line col (inc col))
            is-word? (boolean (re-find re ch))]
        (if is-word?
          (let [word-start (loop [c col] (if (and (> c 0) (re-find re (.slice line (dec c) c))) (recur (dec c)) c))
                word-end (loop [c col] (if (and (< c (.-length line)) (re-find re (.slice line c (inc c)))) (recur (inc c)) c))]
            {:start {:row row :col word-start} :end {:row row :col word-end}})
          (let [nonword-start (loop [c col] (if (and (> c 0) (not (re-find re (.slice line (dec c) c)))) (recur (dec c)) c))
                nonword-end (loop [c col] (if (and (< c (.-length line)) (not (re-find re (.slice line c (inc c))))) (recur (inc c)) c))]
            {:start {:row row :col nonword-start} :end {:row row :col nonword-end}}))))))

(defn find-quote-bounds [text cursor target-quote]
  (let [lines (.split text "\n")
        row (:row cursor) col (:col cursor)
        line (aget lines row)]
    (if (or (nil? line) (>= col (.-length line)))
      nil
      (let [ch (.slice line col (inc col))
            quote (if (= target-quote ch) ch nil)]
        (if quote
          (let [open-pos (loop [c (dec col)] (if (< c 0) nil (if (= (.slice line c (inc c)) target-quote) c (recur (dec c)))))
                close-pos (loop [c (inc col)] (if (>= c (.-length line)) nil (if (= (.slice line c (inc c)) target-quote) c (recur (inc c)))))]
            (if (and open-pos close-pos)
              {:start {:row row :col open-pos} :end {:row row :col (inc close-pos)} :quote target-quote}
              (if open-pos
                {:start {:row row :col open-pos} :end {:row row :col (inc col)} :quote target-quote}
                (if close-pos
                  {:start {:row row :col col} :end {:row row :col (inc close-pos)} :quote target-quote}
                  nil))))
          (let [open-pos (loop [c col]
                           (if (< c 0) nil
                               (if (= (.slice line c (inc c)) target-quote) c (recur (dec c)))))
                close-pos (when open-pos
                            (loop [c (inc open-pos)]
                              (if (>= c (.-length line)) nil
                                  (if (= (.slice line c (inc c)) target-quote) c (recur (inc c))))))]
            (if (and open-pos close-pos)
              {:start {:row row :col open-pos} :end {:row row :col (inc close-pos)} :quote target-quote}
              (let [fwd-open (loop [c col]
                               (if (>= c (.-length line)) nil
                                   (if (= (.slice line c (inc c)) target-quote) c (recur (inc c)))))
                    fwd-close (when fwd-open
                                (loop [c (inc fwd-open)]
                                  (if (>= c (.-length line)) nil
                                      (if (= (.slice line c (inc c)) target-quote) c (recur (inc c))))))]
                (if (and fwd-open fwd-close)
                  {:start {:row row :col fwd-open} :end {:row row :col (inc fwd-close)} :quote target-quote}
                  nil)))))))))

(defn find-bracket-bounds [text cursor target-key]
  (let [lines (.split text "\n")
        row (:row cursor) col (:col cursor)
        line (aget lines row)]
    (if (or (nil? line) (>= col (.-length line)))
      nil
      (let [pairs {"(" ")" ")" "(" "[" "]" "]" "[" "{" "}" "}" "{" "<" ">" ">" "<"}
            openers #{"(" "[" "{" "<"}
            target-open (if (contains? openers target-key) target-key (get pairs target-key))
            target-close (get pairs target-open)]
        (let [open-pos (loop [c col]
                         (if (< c 0) nil
                             (if (= (.slice line c (inc c)) target-open) c (recur (dec c)))))
              close-pos (when open-pos
                          (loop [i (inc open-pos) depth 1]
                            (if (>= i (.-length line)) nil
                                (let [c (.slice line i (inc i))]
                                  (cond (= c target-open) (recur (inc i) (inc depth))
                                        (= c target-close) (if (= depth 1) i (recur (inc i) (dec depth)))
                                        :else (recur (inc i) depth))))))]
          (if close-pos
            {:start {:row row :col open-pos} :end {:row row :col (inc close-pos)} :bracket target-open}
            (let [fwd-open (loop [c col]
                             (if (>= c (.-length line)) nil
                                 (if (= (.slice line c (inc c)) target-open) c (recur (inc c)))))
                  fwd-close (when fwd-open
                              (loop [i (inc fwd-open) depth 1]
                                (if (>= i (.-length line)) nil
                                    (let [c (.slice line i (inc i))]
                                      (cond (= c target-open) (recur (inc i) (inc depth))
                                            (= c target-close) (if (= depth 1) i (recur (inc i) (dec depth)))
                                            :else (recur (inc i) depth))))))]
              (if fwd-close
                {:start {:row row :col fwd-open} :end {:row row :col (inc fwd-close)} :bracket target-open}
                nil))))))))

(defn text-object-bounds [text cursor type key]
  (cond
    (= key "w")
    (let [bounds (word-bounds text cursor)
          start-col (:col (:start bounds))
          end-col (:col (:end bounds))]
      (if (= type "inner")
        bounds
        (let [lines (.split text "\n") row (:row cursor) line (aget lines row)]
          (if (and (< end-col (.-length line)) (= " " (.slice line end-col (inc end-col))))
            {:start (:start bounds) :end {:row row :col (inc end-col)}}
            (if (and (> start-col 0) (= " " (.slice line (dec start-col) start-col)))
              {:start {:row row :col (dec start-col)} :end (:end bounds)}
              bounds)))))
    (contains? #{"\"" "'" "`"} key)
    (let [qb (find-quote-bounds text cursor key)]
      (if qb
        (if (= type "inner")
          {:start {:row (:row (:start qb)) :col (inc (:col (:start qb)))}
           :end {:row (:row (:end qb)) :col (dec (:col (:end qb)))}}
          qb)
        nil))
    :else
    (let [bb (find-bracket-bounds text cursor key)]
      (if bb
        (if (= type "inner")
          {:start {:row (:row (:start bb)) :col (inc (:col (:start bb)))}
           :end {:row (:row (:end bb)) :col (dec (:col (:end bb)))}}
          bb)
        nil))))

;; ============================================================
;; Keyboard event dispatcher
;; ============================================================
(def last-key-time (atom 0))
(def pending-d (atom false))
(def pending-g (atom false))
(def pending-find (atom nil))
(def last-find (atom nil))

(defn handle-key [e]
  (let [key (.-key e)]
    ;; Ignore modifier keys so they don't break multi-key ops like ci"
    (when-not (contains? #{"Shift" "Control" "Alt" "Meta" "AltGraph" "CapsLock"} key)
      (let [ctrl (.-ctrlKey e)
            state @app-state
            mode (:mode state)
            op (:operator state)
            tot (:text-object-type state)]
        (.preventDefault e)
        (cond
          ;; 1. Escape / Cancel
          (or (= key "Escape") (and ctrl (= key (str (char 91)))))
          (do (reset! pending-d false) (reset! pending-g false) (reset! pending-find nil)
              (if (= mode "visual")
                (swap! app-state assoc :mode "normal" :message "" :visual-start nil :visual-type nil :operator nil :text-object-type nil :command-text "")
                (swap! app-state assoc :mode "normal" :message "" :operator nil :text-object-type nil :command-text "" :last-search nil))
              (render))

          ;; 2. Insert Mode
          (= mode "insert")
          (do (cond
                (and ctrl (= key "w")) (swap! app-state cmd-delete-word-backward)
                (= key "Enter") (swap! app-state cmd-insert-newline)
                (= key "Backspace") (swap! app-state cmd-delete-backward)
                (= (.-length key) 1) (swap! app-state cmd-insert-text key))
              (render))

          ;; 3. Command Mode
          ;; 3. Command Mode
          (= mode "command")
          (do (case key
                "w" (do
                      (.setItem js/localStorage storage-key (:text state))
                      (swap! app-state assoc :message "Saved to localStorage"))
                "q" (swap! app-state assoc :message "Quit? Just close the tab")
                (swap! app-state assoc :message (str "Unknown cmd: :" key)))
              (when (or (= key "Enter") (= key "w") (= key "q"))
                (swap! app-state assoc :mode "normal"))
              (render))

          ;; 4. Search Mode (Buffer for '/' and '?')
          (= mode "search")
          (do (cond
                (= key "Enter")
                (let [cmd-text (:command-text state)
                      dir (if (.startsWith cmd-text "?") :backward :forward)
                      query (.slice cmd-text 1)]
                  (swap! app-state (fn [s] (-> s
                                               (assoc :mode "normal" :command-text "")
                                               (cmd-execute-search query dir)))))
                (= key "Backspace")
                (if (<= (.-length (:command-text state)) 1)
                  (swap! app-state assoc :mode "normal" :command-text "")
                  (swap! app-state assoc :command-text (.slice (:command-text state) 0 -1)))
                (= (.-length key) 1)
                (swap! app-state assoc :command-text (str (:command-text state) key)))
              (render))

          ;; 5. Visual Mode
          (= mode "visual")
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
              "e" (if @pending-g
                    (do (reset! pending-g false) (swap! app-state cmd-move-word-end-backward))
                    (swap! app-state cmd-move-word-end-forward))
              "d" (swap! app-state cmd-delete-selection)
              "x" (swap! app-state cmd-delete-selection)
              "y" (swap! app-state cmd-yank-selection)
              "g" (do (reset! pending-g true) (js/setTimeout (fn [] (reset! pending-g false)) 500))
              (swap! app-state assoc :message (str "Unknown: " key)))
            (render))

          ;; 6. Normal Mode: Visual Block entry
          (and ctrl (= key "v"))
          (do (swap! app-state cmd-enter-visual "block") (render))

          ;; 7. Normal Mode: Text Object Pending (ci", yiw, etc)
          tot
          (do (let [obj-keys #{"w" "\"" "'" "`" "(" ")" "[" "]" "{" "}" "<" ">"}]
                (if (contains? obj-keys key)
                  (let [bounds (text-object-bounds (:text state) (:cursor state) tot key)]
                    (if bounds
                      (swap! app-state apply-operator-to-bounds op bounds)
                      (swap! app-state assoc :operator nil :text-object-type nil :message "No bounds found")))
                  (swap! app-state assoc :operator nil :text-object-type nil)))
              (render))

          ;; 8. Normal Mode: Operator Pending (d, c, y, cs, ds, ys)
          op
          (do (cond
                (or (= key "i") (= key "a"))
                (swap! app-state assoc :text-object-type (if (= key "i") "inner" "around"))

                (and (= op "delete") (= key "s"))
                (swap! app-state assoc :operator "delete-surround")

                (= op "delete-surround")
                (do (swap! app-state cmd-delete-surround key) (swap! app-state assoc :operator nil))

                (and (= op "change") (= key "s"))
                (swap! app-state assoc :operator "change-surround-old")

                (= op "change-surround-old")
                (swap! app-state assoc :operator "change-surround-new" :surround-old key)

                (= op "change-surround-new")
                (do (swap! app-state (fn [s] (cmd-change-surround s (:surround-old s) key)))
                    (swap! app-state assoc :operator nil :surround-old nil))

                (and (= op "yank") (= key "s"))
                (swap! app-state assoc :operator "add-surround-motion")

                (and (= op "add-surround-motion") (= key "s"))
                (swap! app-state (fn [s]
                                   (let [row (:row (:cursor s))
                                         len (line-length (get-lines s) row)]
                                     (assoc s :operator "add-surround-char"
                                              :surround-bounds {:start {:row row :col 0} :end {:row row :col len}}))))

                (= op "add-surround-char")
                (swap! app-state cmd-apply-add-surround key)

                (motion-key? key)
                (let [motion-s (apply-motion state key)] (swap! app-state apply-operator op motion-s))

                (contains? #{"f" "F" "t" "T"} key)
                (do (reset! pending-find {:type key :dir (case key ("f" "t") :forward ("F" "T") :backward) :operator op}))

                (and (= op "delete") (= key "d"))
                (swap! app-state cmd-delete-line)

                (and (= op "change") (= key "c"))
                (do (swap! app-state cmd-delete-line) (swap! app-state assoc :mode "insert"))

                (and (= op "yank") (= key "y"))
                (swap! app-state cmd-yank-line)

                :else
                (swap! app-state assoc :operator nil :message ""))
              (render))

          ;; 9. Normal Mode: Pending Find (f, F, t, T)
          @pending-find
          (do (let [fi @pending-find char key pre-state @app-state]
                (reset! pending-find nil)
                (case (:type fi)
                  "f" (do (swap! app-state cmd-find-char-forward char) (reset! last-find {:char char :type "f" :dir :forward}))
                  "F" (do (swap! app-state cmd-find-char-backward char) (reset! last-find {:char char :type "F" :dir :backward}))
                  "t" (do (swap! app-state cmd-find-till-forward char) (reset! last-find {:char char :type "t" :dir :forward}))
                  "T" (do (swap! app-state cmd-find-till-backward char) (reset! last-find {:char char :type "T" :dir :backward})))
                (when (:operator fi)
                  (let [post-state @app-state] (reset! app-state (apply-operator pre-state (:operator fi) post-state)))))
              (render))

          ;; 10. Normal Mode: Default keystrokes
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
                "e" (if @pending-g
                      (do (reset! pending-g false) (swap! app-state cmd-move-word-end-backward))
                      (swap! app-state cmd-move-word-end-forward))
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

                ;; ---> SEARCH BINDINGS <---
                "/" (swap! app-state assoc :mode "search" :command-text "/")
                "?" (swap! app-state assoc :mode "search" :command-text "?")
                "n" (swap! app-state #(if (:last-search %) (cmd-execute-search % (:last-search %) :forward) (assoc % :message "No previous regular expression")))
                "N" (swap! app-state #(if (:last-search %) (cmd-execute-search % (:last-search %) :backward) (assoc % :message "No previous regular expression")))

                (swap! app-state assoc :message (str "Unknown: " key)))
              (render)))))))

;; ============================================================
;; Character-By-Character Line Rendering
;; ============================================================
(defn render-line-html [line row state]
  (let [query (:last-search state)
        cursor (:cursor state)
        is-cursor-row (= row (:row cursor))
        cursor-col (:col cursor)
        mode (:mode state)
        vstart (:visual-start state)
        visual-mode? (and (= mode "visual") vstart)
        bounds (when visual-mode? (visual-bounds state))
        vtype (:visual-type state)

        in-visual? (fn [r c]
                     (if-not bounds false
                       (let [sr (:row (:start bounds))
                             sc (:col (:start bounds))
                             er (:row (:end bounds))
                             ec (:col (:end bounds))]
                         (case vtype
                           "line" false ;; Handled at the top level div via .vim-visual-block
                           "block" (and (>= r sr) (<= r er) (>= c sc) (<= c ec))
                           (if (= sr er)
                             (and (= r sr) (>= c sc) (<= c ec))
                             (cond
                               (= r sr) (>= c sc)
                               (= r er) (<= c ec)
                               :else (and (> r sr) (< r er))))))))

        q-len (if query (.-length query) 0)
        ;; Pre-calculate indices of all chars that match the search string
        matches (if (and query (> q-len 0))
                  (loop [idx 0 acc #{}]
                    (let [found (.indexOf line query idx)]
                      (if (>= found 0)
                        (recur (+ found q-len) (into acc (range found (+ found q-len))))
                        acc)))
                  #{})

        line-len (.-length line)
        ;; Allow rendering past EOL for insert cursor or visual blocks
        render-len (let [c-len (if (and is-cursor-row (>= cursor-col line-len)) (inc cursor-col) line-len)]
                     (if (and visual-mode? (= vtype "block") bounds (>= row (:row (:start bounds))) (<= row (:row (:end bounds))))
                       (max c-len (inc (:col (:end bounds))))
                       c-len))]

    (if (= render-len 0)
      (if is-cursor-row
        (if (= mode "normal")
          "<span class='vim-cursor-block'> </span>"
          "<span class='vim-cursor-line'>|</span> ")
        "")
      (let [html-parts #js []]
        (loop [col 0]
          (when (< col render-len)
            (let [char (if (< col line-len) (.charAt line col) " ")
                  is-cursor (and is-cursor-row (= col cursor-col))
                  is-visual (in-visual? row col)
                  is-search (contains? matches col)
                  esc-char (escape-html char)
                  ;; Layer styles (search -> visual -> raw)
                  styled-char (cond
                                is-visual (str "<span class='vim-visual-block'>" esc-char "</span>")
                                is-search (str "<span class='vim-search-hl' style='background-color: #fbbc04; color: #000; border-radius: 2px;'>" esc-char "</span>")
                                :else esc-char)
                  ;; Layer cursor on top
                  final-char (if is-cursor
                               (if (= mode "normal")
                                 (str "<span class='vim-cursor-block'>" esc-char "</span>")
                                 (str "<span class='vim-cursor-line'>|</span>" styled-char))
                               styled-char)]
              (.push html-parts final-char)
              (recur (inc col)))))
        (.join html-parts "")))))

;; ============================================================
;; UI Rendering
;; ============================================================
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
        visual-mode? (and (= mode "visual") vstart)
        bounds (when visual-mode? (visual-bounds state))]

    (set! (.-innerHTML (js/document.getElementById "app"))
      (str "<div class='vim-container'>"
           "<div class='vim-left'>"
           "<div class='vim-header'>EDITOR - "
           (.toUpperCase mode) " MODE</div>"
           "<div class='vim-editor' id='vim-editor' contenteditable='false'>"

           ;; Rendering Loop simplified using character mapping
           (.join (.map lines (fn [line i]
                                (let [in-visual-line? (and visual-mode?
                                                           (= vtype "line")
                                                           bounds
                                                           (>= i (:row (:start bounds)))
                                                           (<= i (:row (:end bounds))))]
                                  (str "<div class='vim-line"
                                       (if (= i cursor-row) " vim-current-line" "")
                                       (if in-visual-line? " vim-visual-block" "")
                                       "'>"
                                       (render-line-html line i state)
                                       "</div>"))))
                  "")

           "</div>"
           "<div class='vim-statusbar'>"
           (if (= mode "search")
             (str "<span class='vim-cmdline'>" (escape-html (:command-text state)) "<span class='vim-cursor-block'>&nbsp;</span></span>")
             (str "<span class='vim-mode-" mode "'>" (.toUpperCase mode) "</span>"
                  (if (not= msg "") (str " <span class='vim-msg'>" msg "</span>") "")))
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
           "</div>"))

    ;; --- BUG FIX: Auto-scroll to keep the cursor in view ---
    (let [cursor-el (or (.querySelector js/document ".vim-cursor-block")
                        (.querySelector js/document ".vim-cursor-line")
                        (.querySelector js/document ".vim-current-line"))]
      (when cursor-el
        ;; Using block: "nearest" ensures smooth scrolling only when the cursor
        ;; actually pushes past the visible boundary of the #vim-editor container.
        (.scrollIntoView cursor-el #js {:block "nearest" :inline "nearest"})))))


(defn init []
  (js/document.addEventListener "keydown" handle-key)
  (render)
  (println "Vim Org Editor initialized!"))

(init)
