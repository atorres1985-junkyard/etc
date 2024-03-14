;;; Taken from https://github.com/Crandel/tempel-collection

css-base-mode

emacs-lisp-mode

(autoload
  & ";;;###autoload")

(pt
 "(point)")

(var
 "(defvar " p "\n  \"" p "\")")

(local
 "(defvar-local " p "\n  \"" p "\")")

(const
 "(defconst " p "\n  \"" p "\")")

(custom
 "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")

(face
 "(defface " p " '((t :inherit " p "))\n  \"" p "\")")

(group
 "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")

(macro
 "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")

(alias
 "(defalias '" p " '" p ")")

(fun
 "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")

(iflet
 "(if-let (" p ")" n> r> ")")

(whenlet
 "(when-let (" p ")" n> r> ")")

(iflet*
 "(if-let* (" p ")" n> r> ")")

(whenlet*
 "(when-let* (" p ")" n> r> ")")

(andlet*
 "(and-let* (" p ")" n> r> ")")

(cond
 "(cond" n "(" q "))" >)

(pcase
    "(pcase " (p "scrutinee") n "(" q "))" >)

(let
    "(let (" p ")" n> r> ")")

(let*
    "(let* (" p ")" n> r> ")")

(rec
 "(letrec (" p ")" n> r> ")")

(dotimes
    "(dotimes (" p ")" n> r> ")")

(dolist
    "(dolist (" p ")" n> r> ")")

(loop
 "(cl-loop for " p " in " p " do" n> r> ")")

(command
 "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")

(advice
 "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
 "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")

(provide
 "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
 ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)

(log
 "(message \"LOG %s " (s sexp) ": %S"
 "\" (format-time-string \"%X\") " sexp ")")

eshell-mode

;; taken from https://github.com/minad/tempel/
(for
 "for " (p "i") " in " p " { " q " }")

(while
    "while { " p " } { " q " }")

(until
 "until { " p " } { " q " }")

(if
    "if { " p " } { " q " }")

(ife
 "if { " p " } { " p " } { " q " }")

(unl
 "unless { " p " } { " q " }")

(unle
 "unless { " p " } { " p " } { " q " }")

fundamental-mode ;; Available everywhere

(today
 (format-time-string "%Y-%m-%d"))

(NOW
 (format-time-string "<%Y-%0m-%0d %a %H:%0M>"))

(yesterday
 (format-time-string "<%Y-%0m-%0d %a>" (time-subtract nil (* 24 60 60))))

(tomorrow
 (format-time-string "<%Y-%0m-%0d %a>" (time-add nil (* 24 60 60))))

go-mode go-ts-mode

(imp
 "import " q)

(impn
 "import (" n> q n ")")

(pr
 "fmt.Printf(\"\\n" p "\\n%#v\\n\", " q ")")

(pl
 "fmt.Println(" q ")")

(db
 "Debug.Printf(\"\\n" p "\\n\\n%#v\\n\", " q ")")

(dl
 "Debug.Println(" q ")")

(lf
 "log.Printf(\"\\n%#v\\n\", " q ")")

(ln
 "log.Println(" q ")")

(stt
 "type " p " struct {" n> q n "}")

(inf
 "type " p " interface {" n> q n "}")

(cnt
 "const " p " = " q )

(cnst
 "const (" n> p " = " q n ")")

(vr
 "var " p " " q)

(mp
 "map[" p "]" q)

(if
    "if " p " {" n> p n> "}" q)

(el
 "if " p " {" n> p n> "} else {" n> p n> "}" q)

(elif
 "if " p " {" n> p n> "} else if " p " {" n> p n> "}" q)

(ifen
 "if err != nil {" n> q n> "}")

(ifer
 "if err != " p " {" n> q n> "}")

(sel
 "select {" n> "case " p ":" n> q n> "}")

(swch
 "switch " p " {" n> "case " p ":" q n> "}")

(fr
 "for " p "{" n> q n> "}")

(rng
 "for " p ", " p " := range " p " {" n> q n> "}")

(fnc
 "func " p "(" p ") {" n> q n "}")

(mn
 "func main() {" n> q n "}")

(in
 "func init() {" n> q n "}")

(tst
 "func Test" p " (t *testing.T) { " n> q n "}")

html-mode

java-mode

(pkg
 "package " q ";")

(imp
 "import " q ";")

(pl
 "System.out.println(\"" q "\");")

(class
 "public class " p " {" n> q n "}")

(main
 "public static void main(String[] args) {" n> q n> "}")

(if
    "if (" p ") {" n> q n> "}")

(elif
 "else if (" p ") {" n> q n> "}")

(else
 "else {" n> q n> "}")

(for
 "for (" p ") {" n> q n> "}")

js-base-mode

(if
    "if (" p ") {" n> q n> "}")

(el
 "if (" p ") {" n> p n> "} else { " n> q n> "}")

(elif
 "else if (" p ") {" n> q n> "}")

(else
 "else {" n> q n> "}")

(cop
 "( " p ") ? " p " : " q";")

(switch
 "switch (" p ") {" n> "case " p ":" n> p n> "default:" n> q n> "}")

(case
 "case " p ":" n> q)

(while
    "while (" p ") {" n> q n> "}")

(dowhile
 "do {" n> p n> "} while (" q ");")

(for
 "for (" p "; " p "; " p ") {" n> q n> "}")

(fin
 "for (" p " in " p ") {" n> q n> "}")

(fof
 "for (" p " of " p ") {" n> q n> "}")

(func
 "function " p "(" p ") {" n> q n>"}")

(fexp
 "let " p " = function(" p ") {" n> q n> "};")

(imp
 "import {" p "} from \"" q "\";")

(impas
 "import {" p " as " p "} from \"" q "\";")

latex-mode

;; taken from https://github.com/minad/tempel/
(abstract
 "\\begin{abstract}\n" r> n> "\\end{abstract}")

(align
 "\\begin{align}\n" r> n> "\\end{align}")

(alignn
 "\\begin{align*}\n" r> n> "\\end{align*}")

(gather
 "\\begin{gather}\n" r> n> "\\end{gather}")

(gatherr
 "\\begin{gather*}\n" r> n> "\\end{gather*}")

(appendix
 "\\begin{appendix}\n" r> n> "\\end{appendix}")

(begin
 "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")

(center
 "\\begin{center}\n" r> n> "\\end{center}")

(displaymath
 "\\begin{displaymath}\n" r> n> "\\end{displaymath}")

(document
 "\\begin{document}\n" r> n> "\\end{document}")

(enumerate
 "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")

(equation
 "\\begin{equation}" r> n> "\\end{equation}")

(flushleft
 "\\begin{flushleft}" r> n> "\\end{flushleft}")

(flushright
 "\\begin{flushright}" r> n> "\\end{flushright}")

(frac
 "\\frac{" p "}{" q "}")

(fussypar
 "\\begin{fussypar}" r> n> "\\end{fussypar}")

(itemize
 "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

(letter
 "\\begin{letter}\n" r> n> "\\end{letter}")

(math
 "\\begin{math}\n" r> n> "\\end{math}")

(minipage
 "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}")

(quotation
 "\\begin{quotation}\n" r> n> "\\end{quotation}")

(quote
 "\\begin{quote}\n" r> n> "\\end{quote}")

(sloppypar
 "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}")

(theindex
 "\\begin{theindex}\n" r> n> "\\end{theindex}")

(trivlist
 "\\begin{trivlist}\n" r> n> "\\end{trivlist}")

(verbatim
 "\\begin{verbatim}\n" r> n> "\\end{verbatim}")

(verbatimm
 "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}")

lisp-mode emacs-lisp-mode

;; taken from https://github.com/minad/tempel/
(lambda
  "(lambda (" p ")" n> r> ")")

markdown-mode

(gitcollapse
 & "## " (p "Heading") n n "<details>" n n
 "<summary>" (p "Sub Heading")  "</summary>" n n
 (r "Insert Link or comments") n n "</details>")

(bolditalics
 "***" p "***")

(srcblock
 (call-interactively #'markdown-insert-gfm-code-block))

(src
 "'" p "'")

(unorderlist
 & "- " (p "First") n> "- " (p "Second") n> "- " (p "Third"))

(orderlist
 & "1. " (p "First") n> "2. " (p "Second") n> "3. " (p "Third"))

(insertimage
 (call-interactively #'markdown-insert-image))

(insertlink
 (call-interactively #'markdown-insert-link))

(hugotitle
 & "+++" n "title = " (p "title") n "date = " (format-time-string "%Y-%m-%d") n "tags = [ " (p "tag1, tag2 ") "]" n "draft = false" n "+++")

(h1
 & "# " p " #")

(h2
 & "## " p " ##")

(h3
 & "### " p " ###")

(h4
 & "#### " p " ####")

(inserttable
 (call-interactively #'markdown-insert-table))

nix-mode



org-mode

(ttl
 & "#+title: " p n "#+author: " p n "#+language: " p n n)

(qt
 & "#+begin_quote" n> r> n> "#+end_quote")

(exmpl
 & "#+begin_example" n> r> n> "#+end_example")

(cntr
 & "#+begin_center" n> r> n> "#+end_center")

(comm
 & "#+begin_comment" n> r> n> "#+end_comment")

(vrs
 & "#+begin_verse" n> r> n> "#+end_verse")

(src
 & "#+begin_src " p n r n "#+end_src" :post (org-edit-src-code))

(rst
 & "#+begin_src restclient" p n r n "#+end_src" :post (org-edit-src-code))

(elsp
 & "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))

(readonly
 ":tangle yes :tangle-mode (identity #o444) :mkdirp yes" n)

(oxhugo
 & ":PROPERTIES:"  n ":EXPORT_FILE_NAME: " (p "Simple Filename") n ":EXPORT_DATE: " (format-time-string "%Y-%m-%d") n ":EXPORT_HUGO_DRAFT: false" n ":END:")

(readmecollapse
 "*** " (p "Heading") n "#+HTML: <details> <summary> " (p "sub-heading")  " </summary>" n (r> "link or any comments") n n "#+HTML: </details>" n)

;; taken from https://github.com/minad/tempel/blob/
(caption
 & "#+caption: ")

(drawer
 & ":" p ":" n r ":end:")

(begin
 & "#+begin_" (s name) n r n "#+end_" name)

(quote
 & "#+begin_quote" n> r> n "#+end_quote")

(sidenote
 & "#+begin_sidenote" n> r> n "#+end_sidenote")

(marginnote
 & "#+begin_marginnote" n> r> n "#+end_marginnote")

(example
 & "#+begin_example" n> r> n "#+end_example")

(center
 & "#+begin_center" n> r> n "#+end_center")

(ascii
 & "#+begin_export ascii" n> r> n "#+end_export")

(html
 & "#+begin_export html" n> r> n "#+end_export")

(latex
 & "#+begin_export latex" n> r> n "#+end_export")

(comment
 & "#+begin_comment" n> r> n "#+end_comment")

(verse
 & "#+begin_verse" n> r> n "#+end_verse")

(gnuplot
 & "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))

(elisp
 & "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))

(inlsrc
 "src_" p "{" q "}")

(title
 & "#+title: " p n "#+author: " p n "#+language: " p n n)

prog-mode

;; taken from https://github.com/minad/tempel/blob/5b09f612cfd805dba5e90bf06580583cab045499/README.org#template-file-format (GPL-3.0 license)

(fixme
 (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")

(todo
 (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")

(bug
 (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")

(hack
 (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

python-base-mode

(for
 "for " p " in " p ":" n> q)

(tr
 & "import " p "; " p ".set_trace()" q)

(ig
 "# type: ignore" q)

rst-mode

;; taken from https://github.com/minad/tempel/blob/5b09f612cfd805dba5e90bf06580583cab045499/README.org#template-file-format (GPL-3.0 license)

(title
 (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

ruby-base-mode

rust-mode rust-ts-mode

(if
    "if " p " {" n> q n> "}" q)

(el
 "if " p " {" n> p n> "} else {" n> p n> "}" q)

(elif
 "if " p " {" n> p n> "} else if " p " {" n> p n> "}" q)

(fn
 "fn " p "(" p ") {" n> p n "}" q)

(fnr
 "fn " p "(" p ") -> " p " {" n> p n "}" q)

(crt
 "extern crate " q ";")

(drv
 & "#[derive(" p ")]" q)

(d
 "dbg!(" p ")" q)

(lp
 "loop {" n> q n> "}" q)

sh-base-mode

(!
 & "#!/usr/bin/env bash" n q)

(if
    "if [ " p " ]" n "  then " p n "fi" q)

(ife
 "if [ " p " ]" n "  then " p n "else" p n "fi" q)

(cs
 "case " p " in" n> p " )" n> p n> ";;" n> q n "esac")

(fr
 "for " p " in " p n "do" n> q n "done")

texinfo-mode

;; taken from https://github.com/minad/tempel/
(defmac
 "@defmac " p n> r> "@end defmac")

(defun
    "@defun " p n> r> "@end defun")

(defvar
  "@defvar " p n> r> "@end defvar")

(example
 "@example " p n> r> "@end example")

(lisp
 "@lisp " p n> r> "@end lisp")

(bullet
 "@itemize @bullet{}" n> r> "@end itemize")

(code
 "@code{" p "}")

(var
 "@var{" p "}")

text-mode

;; taken from https://github.com/minad/tempel/
(box
 "┌─" (make-string (length str) ?─) "─┐" n
 "│ " (s str)                       " │" n
 "└─" (make-string (length str) ?─) "─┘" n)

(abox
 "+-" (make-string (length str) ?-) "-+" n
 "| " (s str)                       " |" n
 "+-" (make-string (length str) ?-) "-+" n)
