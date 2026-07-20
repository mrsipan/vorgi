# Vorgi — Vim Org Editor

A **Vim-like editor** with live **Org-mode preview**, running entirely in the browser. Built with [Squint ClojureScript](https://github.com/squint-cljs/squint).

## ✨ Features

### Vim Editing
- **Modes** — Normal, Insert, Visual (char/line/block), Command
- **Motions** — `h` `j` `k` `l` · `w` `b` `e` · `0` `$` · `gg` `G` · `f` `F` `t` `T` · `;` `,`
- **Operators** — `d` `c` `y` with motions and text objects
- **Text objects** — `iw` `aw` · `i"` `a"` · `i(` `a(` · `i[` `a[` · `i{` `a{`
- **Visual mode** — `v` (char) · `V` (line) · `Ctrl+v` (block)
- **Editing** — `i` `a` `I` `A` · `o` `O` · `x` · `D` `Y` · `p` `P` · `u` (undo)
- **Surround** — `ds` to delete surrounding pairs/quotes

### Org-mode Preview
- Real-time rendered preview of Org-mode markup
- Supports headings, paragraphs, bold/italic/code, and lists

## 🚀 Quick Start

```
git clone git@github.com:mrsipan/vorgi.git
cd vorgi
python3 -m http.server 8080
# Open http://localhost:8080
```

Or just open `index.html` directly — it's a single-page app with no build step.

## 🛠️ Tech

| Layer | Tech |
|-------|------|
| Language | [Squint CLJS](https://github.com/squint-cljs/squint) (ClojureScript compiled at runtime) |
| Org parsing | [Orga](https://github.com/orgapp/orgajs) |
| Hosting | GitHub Pages |
| CI/CD | GitHub Actions |

## 📝 License

MIT
