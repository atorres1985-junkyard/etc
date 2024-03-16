;;; init.el --- Emacs init file -*- lexical-binding: t; no-byte-compile: nil -*-

;;; Commentary:
;;  Arquivo de inicialização do Emacs

;;; Code:

(defconst atorres-1985|init-start-time (current-time)
  "Init start time.")

(require 'cl-lib)
(require 'package)

(package-initialize)

(require 'setup)

(unless (getenv "EDITOR")
  (setenv "EDITOR" "emacsclient"))

(unless (getenv "VISUAL")
  (setenv "VISUAL" "emacsclient"))

(unless (getenv "PAGER")
  (setenv "PAGER" "cat"))

(defconst home-directory (getenv "HOME")
  "Home directory.")

(defconst user-emacs-directory-log (concat user-emacs-directory "log/")
  "Logs directory.")

(setup no-littering
  (setq
   no-littering-etc-directory (expand-file-name ".cache/emacs/etc/" home-directory)
   no-littering-var-directory (expand-file-name ".cache/emacs/var/" home-directory))
  (:require no-littering)
  (dolist (dir `(,no-littering-etc-directory
         ,no-littering-var-directory))
    (unless (file-exists-p dir)
  (make-directory dir t))))

(defmacro atorres-1985|emit-backup-pair (extension directory)
  "Return a pair (regex . directory) suitable for use as `backup-directory-alist'.

The car of this pair is `EXTENSION' in regexp form, and the cdr is a
subdirectory of `DIRECTORY' named `EXTENSION'."

  `(cons (rx "." ,extension eol)
         (expand-file-name ,extension ,directory)))

(defmacro atorres-1985|pretty-expanded-form (form)
  "Output pretty-printed expanded form of given `FORM'."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))

(defun atorres-1985|sort-words (reverse begin end)
  "Sort words in region alphabetically, in REVERSE if negative.

Region is defined between BEGIN and END.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the
sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" begin end))

(defun atorres-1985|sort-symbols (reverse begin end)
  "Sort symbols in region alphabetically, in REVERSE if negative.

Region is defined between BEGIN and END.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the
sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" begin end))

(defun atorres-1985|show-percentage ()
  "Calculate and display the percentage before and after the current point."
  (interactive)
  (let* ((frac (/ (* 1.0 (point)) (point-max)))
         (perc (* 100 frac))
         (rest (- 100 perc)))
    (message "atorres-1985|show-percentage: %d + %d = %d"
             perc rest (+ perc rest))))

(defun atorres-1985|move-region (start end n)
  "Move the region bewteen START and END by N lines."
  (interactive "r\np")
  (let
      ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let
        ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun atorres-1985|move-region-up (start end n)
  "Move the region between START and END up by N lines."
  (interactive "r\np")
  (atorres-1985|move-region start end (if (null n) -1 (- n))))

(defun atorres-1985|move-region-down (start end n)
  "Move the region between START and END down by N lines."
  (interactive "r\np")
  (atorres-1985|move-region start end (if (null n) 1 n)))

(defun atorres-1985|toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullboth)))

(defun atorres-1985|find-init-file ()
  "Open Emacs init file."
  (interactive)
  (find-file user-init-file))

(defun atorres-1985|load-directory (directory)
  "Load extra scripts scattered on DIRECTORY."
  (when (file-directory-p directory)
    (dolist (file (directory-files directory nil "el$"))
      (load-file (concat directory "/" file)))))

(defun atorres-1985|add-to-list-after (list-var old new &optional compare-fn)
  "Add NEW after OLD in the ordered list LIST-VAR.

OLD is compared with COMPARE-FN which defaults to `equal'.

NEW is not added if it already exists after OLD, also according
to COMPARE-FN, making this function idempotent."
  (let ((cmp (or compare-fn #'equal)))
    (cl-do ((x (symbol-value list-var) (cdr x)))
        ((null x))
      (when (and (funcall cmp (car x) old)
                 (not (funcall cmp (cadr x) new)))
        (setf (cdr x) (cons new (cdr x))))))
  (symbol-value list-var))

(defun atorres-1985|buffer-file-or-directory-name (buf)
  "The file BUF is visiting; works if it's a `dired' buffer too."
  (with-current-buffer buf
    (or buffer-file-name
        (and (eq major-mode 'dired-mode)
             (boundp 'dired-directory)
             (file-name-directory
              (if (stringp dired-directory)
                  dired-directory
                (car dired-directory)))))))

(defmacro atorres-1985|with-face (string &rest face-properties)
  "A thin wrapper on propertize.

Return a copy of STRING with FACE-PROPERTIES added."
  `(propertize ,string 'face (list ,@face-properties)))

(defun atorres-1985|eshell-prompt ()
  "Custom eshell prompt."
  (let ((header-bg "#000"))
    (concat "\n"
            (atorres-1985|with-face (concat (eshell/pwd) " ")
                                    :background header-bg)
            (atorres-1985|with-face (format-time-string "(%Y-%m-%d %H:%M) "
                                                        (current-time))
                                    :background header-bg
                                    :foreground "#0000FF")
            (atorres-1985|with-face
             (or (ignore-errors
                   (format "(%s)"
                           (vc-responsible-backend default-directory)))
                 "")
             :background header-bg)
            (atorres-1985|with-face " "
                                    :background header-bg)
            (atorres-1985|with-face "|"
                                    :background "grey")
            (atorres-1985|with-face user-login-name
                                    :foreground "blue")
            "@"
            (atorres-1985|with-face "localhost"
                                    :foreground "green")
            (if (= (user-uid) 0)
                (atorres-1985|with-face " #\n> "
                                        :foreground "red")
              (atorres-1985|with-face " $\n> "
                                      :foreground "green")))))

(defun atorres-1985|string-make-clickable (string click-command &optional help-echo)
  "Create a clickable STRING according to \"33.19.8 Defining Clickable Text\".

CLICK-COMMAND is what happens on click and HELP-ECHO or the
documentation string of CLICK-COMMAND is the tooltip."
  (propertize
   string
   'mouse-face 'highlight
   'help-echo (concat "mouse-2: " (or help-echo
                                      (documentation click-command 'raw)))
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-2] click-command)
             (define-key map (kbd "RET") click-command)
             (define-key map [follow-link] 'mouse-face)
             map)))

(defmacro atorres-1985|add-to-local-keymap (&rest definitions)
  "Add the DEFINITIONS to the current buffer's local keymap.

The DEFINITIONS should be an alist of keys (arguments for `kbd')
and commands.

Probably shouldn't be chained too much, as it would create
a deeply nested keymap inheritance structure."
  (let ((map (gensym)))
    `(let ((,map (make-sparse-keymap)))
       (set-keymap-parent ,map (current-local-map))
       ,@(mapcar
          (lambda (def)
            (list 'define-key map (kbd (car def)) `#',(cdr def)))
          definitions)
       (use-local-map ,map))))

(let*
    ((backup-directory
      (expand-file-name ".text-backups" home-directory))
     (c-pair (atorres-1985|emit-backup-pair "c" backup-directory))
     (el-pair (atorres-1985|emit-backup-pair "el" backup-directory))
     (md-pair (atorres-1985|emit-backup-pair "md" backup-directory))
     (nix-pair (atorres-1985|emit-backup-pair "nix" backup-directory))
     (org-pair (atorres-1985|emit-backup-pair "org" backup-directory))
     (py-pair (atorres-1985|emit-backup-pair "py" backup-directory))
     (sh-pair (atorres-1985|emit-backup-pair "sh" backup-directory))
     (tex-pair (atorres-1985|emit-backup-pair "tex" backup-directory))
     (txt-pair (atorres-1985|emit-backup-pair "txt" backup-directory))
     (adoc-pair (atorres-1985|emit-backup-pair "adoc" backup-directory)))
  (setq backup-directory-alist
        `(("." . ,backup-directory)))
  (dolist (l `(,txt-pair
               ,tex-pair
               ,sh-pair
               ,py-pair
               ,org-pair
               ,nix-pair
               ,md-pair
               ,el-pair
               ,c-pair
               ,adoc-pair))
    (unless (file-exists-p (cdr l))
      (make-directory (cdr l) t))
    (cl-pushnew l backup-directory-alist)))

(setup server
  (:require server)
  (unless (server-running-p)
    (server-start)))

(setup files
  (setopt auto-save-default t
          backup-by-copying t
          delete-old-versions t
          kept-new-versions 5
          kept-old-versions 5
          make-backup-files t
          version-control t))

(setup recentf
  (setopt recentf-max-saved-items 50)
  (recentf-mode 1))

(setup simple
  (setq-default line-move-visual nil)
  (setopt column-number-mode 1
        save-interprogram-paste-before-kill t))

(setup mouse
  (setq mouse-yank-at-point t))

(setup mule
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(setup vc-hooks
  (setq-default vc-follow-symlinks t)
  (setq vc-make-backup-files t))

(setup savehist
  (:require savehist)
  (setopt history-length 25)
  (savehist-mode +1))

(setup which-key
  (:require which-key)
  (which-key-mode))

(setup beginend
  (:require beginend)
  (beginend-global-mode))

(setup paren
  (setopt show-paren-delay 0
          show-paren-style 'parenthesis)
  (show-paren-mode t))

(setup bs
  (:require bs)
  (setopt bs-max-window-height 36
          bs-minimal-buffer-name-column 25))

(setup pdf-tools
  (:require pdf-tools)
  (pdf-tools-install))

(setup text-mode
  (:with-hook text-mode
    (:hook turn-on-visual-line-mode
           turn-on-auto-fill)))

(setup crux
  (:require crux))

(setup hungry-delete
  (:require hungry-delete)
  (global-hungry-delete-mode))

(setup hardcore-mode
  (setopt too-hardcore-backspace t
          too-hardcore-return t)
  (:require hardcore-mode)
  (global-hardcore-mode))

(setup unfill)

(setup display-fill-column-indicator
  (:require display-fill-column-indicator)
  (global-display-fill-column-indicator-mode))

(setup nhexl-mode)

(setup helpful)

(setup eshell
  (:if-package eshell)
  (setopt eshell-prompt-function 'atorres1985|eshell-prompt
          eshell-highlight-prompt nil))

(setup eat
  (:if-package eat)
  (setopt eat-eshell-fallback-if-stty-not-available t))

(setup display-line-numbers)

(setup dynamic-ruler)

(setup deadgrep
  (:and (executable-find "rg")
        (:require deadgrep))
  (defalias 'dgrep 'deadgrep
    "Custom alias for deadgrep."))

(setup dwim-shell-command
  (:require dwim-shell-command)
  (:also-load dwim-shell-commands))

(setup all-the-icons
  (:and (display-graphic-p)
    (:require all-the-icons)))

(setup (:require dired dired-x diredfl dirvish)
  (set-face-attribute 'diredfl-dir-name nil :bold t)
  (:with-hook (dired-mode-hook dirvish-directory-view-mode)
    (:hook diredfl-mode))

  (setopt dirvish-use-mode-line t
          dirvish-use-header-line 'global)

  (setopt dirvish-mode-line-format
          '(:left (sort symlink) :right (omit yank index)))

  (setopt dirvish-attributes
          '(all-the-icons
            collapse
            file-size
            file-time
            git-msg
            subtree-state
            vc-state))

  (setopt dirvish-quick-access-entries
          '(("h" "~/"                          "Home")
            ("d" "~/Downloads/"                "Downloads")
            ("p" "/run/media/"                 "Pendrives")
            ("s" "/mnt/source-codes"           "Sources")
            ("t" "~/.local/share/Trash/files/" "TrashCan")))

  (setopt dired-listing-switches
          (string-join '("-l"
                         "--all"
                         "--inode"
                         "--human-readable"
                         "--group-directories-first"
                         "--no-group")
                       " "))

  ;; dirvish previews
  (dirvish-define-preview eza (file)
    "Use `eza' to generate directory preview."
    :require ("eza")
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("eza" "--all"
                 "--bytes"
                 "--color=always"
                 "--group"
                 "--group-directories-first"
                 "--header"
                 "--icons"
                 "--inode"
                 "--long"
                 "--octal-permissions"
                 "--oneline"
                 ,file))))
  (cl-pushnew 'eza dirvish-preview-dispatchers)

  (dirvish-define-preview elinks (file)
    "Use `elinks' to generate HTML preview."
    :require ("elinks")
    (when (string-match (rx (or ".htm" ".html") line-end) file)
      `(shell . ("elinks"
                 "-dump"
                 "-localhost"
                 "-no-numbering"
                 "-no-references"
                 "-default-mime-type" "text/html"
                 "-dump-width" "80"
                 ,file))))
  (cl-pushnew 'elinks dirvish-preview-dispatchers)

  (dirvish-side-follow-mode)
  (dirvish-override-dired-mode +1))

(setup page-break-lines
  (:require page-break-lines)
  (setopt page-break-lines-modes nil
          page-break-lines-max-width 80)

  (dolist (mode '(compilation-mode
                  dashboard-mode
                  emacs-lisp-mode
                  help-mode
                  lisp-mode
                  outline-mode
                  scheme-mode))
    (add-to-list 'page-break-lines-modes mode))
  (page-break-lines-mode))

(setup magit)

(setup dashboard
  (:require dashboard)
  (setopt dashboard-banner-logo-title "Welcome to the Emacs Dashboard!"
          dashboard-center-content nil
          dashboard-page-separator "\n\n"
          dashboard-set-file-icons nil
          dashboard-set-heading-icons nil
          dashboard-show-shortcuts t
          dashboard-items '()
          dashboard-startup-banner 'logo)

  (add-to-list 'dashboard-items '(agenda . 5))
  (add-to-list 'dashboard-items '(bookmarks . 5))
  (add-to-list 'dashboard-items '(projects . 5))
  ;; (add-to-list 'dashboard-items '(recents . 5))
  (add-to-list 'dashboard-items '(registers . 5))

  (dashboard-setup-startup-hook))

(when (display-graphic-p)
  (load-theme 'ef-bio t nil))

(set-face-attribute 'default nil
                    :height 225)

(set-face-attribute 'lazy-highlight nil
                    :foreground "black"
                    :background "green")

(setq initial-frame-alist (quote ((fullscreen . maximized))))
(cl-pushnew '(width . 120) default-frame-alist) ; characters
(cl-pushnew '(height . 36) default-frame-alist) ; lines

(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil t))

(let
    ((extra-lisp-dir (expand-file-name "extra-lisp.d" user-emacs-directory)))
  (when (file-accessible-directory-p extra-lisp-dir)
    (atorres1985|load-directory extra-lisp-dir)))

;; Don't pop up UI dialogs when prompting
(setopt use-dialog-box nil)

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-unset-key (kbd "<insert>"))

(defalias 'yes-or-no-p 'y-or-n-p
  "An alias for y-or-n-p to write less.")

(setopt auto-save-interval 300
        auto-save-timeout 30
        delete-by-moving-to-trash nil
        inhibit-startup-screen t ; startup.el
        visible-bell t)

(setopt
 initial-scratch-message ; startup.el
 (mapconcat 'identity
            '(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
              ";;; This buffer is for notes you don't want to save, and to Emacs Lisp code. ;;;"
              ";;; If you want to create a file, visit it with `M-x find-file' (usually     ;;;"
              ";;; bounded to `C-x C-f'), then insert text in the file's own buffer.        ;;;"
              ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
            "\n"))

(setq-default fill-column 80
              frame-resize-pixelwise t
              indent-tabs-mode nil
              major-mode 'text-mode
              tab-width 4
              window-resize-pixelwise t
              word-wrap t)

(setq frame-title-format
      '(buffer-file-name
        "%b - %f"                        ; File buffer
        (dired-directory dired-directory ; Dired buffer
                         (revert-buffer-function
                          "%b"                                 ; Buffer Menu
                          ("%b - Dir: " default-directory))))) ; Plain buffer

(setq load-prefer-newer t)

(setup bs
  (:require bs)
  (:global [remap list-buffers] bs-show))

(setup display-line-numbers
  (:global (kbd "C-c l l") display-line-numbers-mode))

(setup unfill
  (:global (kbd "C-c u p") unfill-paragraph)
  (:global (kbd "C-c u r") unfill-region)
  (:global (kbd "C-c u t") unfill-toggle))

(setup helpful
  (:require helpful)
  (:global [remap describe-key] helpful-key)
  (:global [remap describe-function] helpful-callable)
  (:global [remap describe-command] helpful-command)
  (:global [remap describe-variable] helpful-variable)
  (:global (kbd "C-c h s") helpful-symbol)
  (:global (kbd "C-c h p") helpful-at-point))

(setup dynamic-ruler
  (:if-package dynamic-ruler)
  ;; (:global (kbd "C-c r v") dynamic-ruler-vertical) ;; bugged!
  (:global (kbd "C-c r h") dynamic-ruler))

(setup (:require dired dired-x diredfl dirvish)
  (:global (kbd "C-c d f") dirvish-fd)
  (:with-map dired-mode-map
    (defun atorres-1985|dired-find-file-other-frame ()
      "In Dired, visit this file or directory in another window."
      (interactive)
      (find-file-other-frame (dired-get-file-for-visit)))
    (:bind (kbd "F") #'atorres-1985|dired-find-file-other-frame))

  (:with-map dirvish-mode-map
    (:bind (kbd "a")   dirvish-quick-access)
    (:bind (kbd "f")   dirvish-file-info-menu)
    (:bind (kbd "y")   dirvish-yank-menu)
    (:bind (kbd "N")   dirvish-narrow)
    (:bind (kbd "^")   dirvish-history-last)
    (:bind (kbd "h")   dirvish-history-jump) ; remapped `describe-mode'
    (:bind (kbd "s")   dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
    ;; (:bind (kbd "v")   dirvish-vc-menu)   ; remapped `dired-view-file'
    (:bind "<tab>" dirvish-subtree-toggle) ;; kbd does not work!
    (:bind (kbd "M-f") dirvish-history-go-forward)
    (:bind (kbd "M-b") dirvish-history-go-backward)
    (:bind (kbd "M-l") dirvish-ls-switches-menu)
    (:bind (kbd "M-m") dirvish-mark-menu)
    (:bind (kbd "M-t") dirvish-layout-toggle)
    (:bind (kbd "M-s") dirvish-setup-menu)
    (:bind (kbd "M-e") dirvish-emerge-menu)
    (:bind (kbd "M-j") dirvish-fd-jump)))

(setup magit
  (:if-package magit)
  (global-unset-key (kbd "C-x g"))
  (:global "C-c g s" magit-status))

(setup dwim-shell-command
  (:if-package dwim-shell-command)
  (:global (kbd "C-c s !") dwim-shell-command))

(setup gcmh
  (:require gcmh)
  (setopt gcmh-verbose nil)
  (gcmh-mode))

(defconst atorres-1985|init-finish-time (current-time)
  "Init finish time.")

(provide 'init)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; End:

;;; init.el ends here
