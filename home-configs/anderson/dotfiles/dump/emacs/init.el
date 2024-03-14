;;; init.el --- Emacs init file -*- lexical-binding: t; no-byte-compile: nil -*-

;;; Commentary:
;;  Emacs init file

;;; Code:

(require 'cl-lib)
(require 'package)

(package-initialize)

(setq use-package-verbose t)
(require 'use-package)

(unless (getenv "EDITOR")
  (setenv "EDITOR" "emacsclient")
  (setenv "VISUAL" "emacsclient"))

(unless (getenv "PAGER")
  (setenv "PAGER" "cat"))

(defconst home-directory (getenv "HOME")
  "Home directory.")

(defconst user-emacs-directory-log (concat user-emacs-directory "log/")
  "Logs directory.")

;;; no-littering should be on top always, since it affects virtually all
;;; packages
(use-package no-littering
  :init
  (setq
   no-littering-etc-directory (expand-file-name ".cache/emacs/etc/" home-directory)
   no-littering-var-directory (expand-file-name ".cache/emacs/var/" home-directory))
  :config
  (dolist (dir `(,no-littering-var-directory
                 ,no-littering-etc-directory))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package files
  :config
  (setq-default require-final-newline t)
  (setq auto-save-default t
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 5
        make-backup-files t
        version-control t))

(use-package simple
  :config
  (setq-default line-move-visual nil)
  (setq column-number-mode 1
        save-interprogram-paste-before-kill t))

(use-package vc-hooks
  :config
  (setq-default vc-follow-symlinks t)
  (setq vc-make-backup-files t))

(use-package mouse
  :config
  (setq mouse-yank-at-point t))

(use-package mule
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package crux)
;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

(use-package meow
  :disabled
  :init
  (defun atorres1985|meow-toggle ()
    (interactive)
    (meow-global-mode 'toggle))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("0" . meow-digit-argument)
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)

     '("a" . meow-append)            '("A" . meow-open-below)
     '("b" . meow-back-word)         '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)            '("D" . meow-backward-delete)
     '("e" . meow-next-word)         '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)  '("G" . meow-grab)
     '("h" . meow-left)              '("H" . meow-left-expand)
     '("i" . meow-insert)            '("I" . meow-open-above)
     '("j" . meow-next)              '("J" . meow-next-expand)
     '("k" . meow-prev)              '("K" . meow-prev-expand)
     '("l" . meow-right)             '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)             '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)              '("Q" . meow-goto-line)
     '("r" . meow-replace)           '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)              '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)         '("W" . meow-mark-symbol)
     '("x" . meow-line)              '("X" . meow-goto-line)
     '("y" . meow-save)              '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  :config
  (meow-setup))

;; (meow-global-mode 1)

(use-package blackout)

(use-package hardcore-mode
  :init
  (setq too-hardcore-backspace t
        too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package unfill)

(use-package savehist
  :config
  (savehist-mode +1))

(use-package buffer-move
  :init
  (setq buffer-move-behavior 'swap
        buffer-move-stay-after-swap t))

(use-package super-save
  :init
  (setq super-save-auto-save-when-idle t
        super-save-remote-file t
        super-save-exclude '(".gpg"))
  :config
  (super-save-mode +1))

(use-package rg
  :if (executable-find "rg")
  :config
  (rg-enable-menu))

(use-package deadgrep
  :disabled
  :if (executable-find "rg")
  :config
  (defalias 'dgrep 'deadgrep
    "Custom alias for deadgrep."))

(use-package dwim-shell-command
  :ensure t
  :bind (("C-c d !" . dwim-shell-command)))

(use-package dwim-shell-commands
  :after dwim-shell-command)

(use-package org
  :init
  (setq org-html-htmlize-font-prefix "org-#"
        org-html-htmlize-output-type 'inline-css
        org-highlight-latex-and-related '(latex script)
        org-link-descriptive nil)
  :config
  (add-hook 'org-mode-hook 'flyspell-mode))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode nil))

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook  #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook  #'paredit-mode)
  (add-hook 'ielm-mode-hook  #'paredit-mode)
  (add-hook 'lisp-mode-hook  #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook  #'paredit-mode)
  (add-hook 'scheme-mode-hook  #'paredit-mode))

(use-package magit)
;; (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
;; (setq magit-revision-insert-related-refs nil)
;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse))

(use-package keypression
  :init
  (setq keypression-use-child-frame nil
        keypression-fade-out-delay 1.0
        keypression-frame-justify 'keypression-left-justified
        keypression-cast-command-name t
        keypression-cast-command-name-format "%s  %s"
        keypression-combine-same-keystrokes t
        keypression-font-face-attribute '(:width normal :height 200 :weight bold)))

(use-package which-key
  :config
  (which-key-mode))

(use-package ace-window
  :init
  (setq
   aw-keys
   '(?1
     ?2
     ?3
     ?4
     ?5
     ?6
     ?7
     ?8
     ?9))
  :config
  (cl-pushnew 'ace-window super-save-triggers)
  :bind
  ("M-o" . ace-window))

(use-package edwina
  :disabled
  :custom
  (edwina-keymap-prefix (kbd "C-c C-w"))
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1))

(use-package page-break-lines
  :init
  (setq page-break-lines-modes nil
        page-break-lines-max-width 80)
  (dolist (mode '(compilation-mode
                  dashboard-mode
                  emacs-lisp-mode
                  help-mode
                  lisp-mode
                  outline-mode
                  scheme-mode))
    (add-to-list 'page-break-lines-modes mode))
  :config
  (page-break-lines-mode))

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Welcome to the Emacs Dashboard!"
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
  :config
  (dashboard-setup-startup-hook))

(use-package helm
  :disabled ; Because I am using vertico & friends
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (helm-mode +1))

;;; BEGIN completion engine

(use-package vertico
  :bind
  (("C-x M-r" . vertico-repeat)
   :map vertico-map
   ("C-l" . vertico-directory-delete-word)
   ("M-g" . vertico-multiform-grid)
   ("M-q" . vertico-multiform-flat))
  :init
  (vertico-mode 1)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mouse-mode 1)
  (vertico-multiform-mode 1)
  (setq vertico-cycle t
        vertico-resize nil
        vertico-multiform-categories '((consult-grep buffer))
        vertico-multiform-commands '((tmm-menubar flat)
                                     (tmm-shortcut flat)))

  ;; Needed with `read-file-name-completion-ignore-case'.
  ;; - https://github.com/minad/vertico/issues/341
  ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60264
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package orderless
  :after vertico
  :config
  (setq orderless-matching-styles '(orderless-regexp
                                    orderless-initialism
                                    orderless-prefixes)
        orderless-component-separator #'orderless-escapable-split-on-space

        ;; Use the built-in "partial-completion" style to complete file inputs
        ;; such as "/e/ni/co.nix" into "/etc/nixos/configuration.nix". The
        ;; "basic" style is needed to support the hostname completion in the
        ;; TRAMP inputs such as "/sshx:HOSTNAME".
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))

        ;; Make the stock file completion styles ("basic" and
        ;; "partial-completion") case insensitive, a better fit for the behavior
        ;; provided by orderless. See the `orderless-smart-case' documentation
        ;; for how it interacts with orderless itself (spoiler: in this setup it
        ;; doesn't).
        read-file-name-completion-ignore-case t
        completion-styles '(orderless basic))

  (defun atorres1985|call-without-orderless-dispatchers (orig &rest args)
    "Use with `advice-add' (`:around') to ignore the dispatchers."
    (let ((orderless-style-dispatchers nil))
      (apply orig args))))

(use-package embark
  :bind
  (("C-c o e" . embark-dwim)
   ("C-."   . embark-act)
   :map minibuffer-local-map
   ("M-o"   . embark-act)
   :map embark-command-map
   ;; Unbind the dangerous `global-set-key' and `local-set-key' actions. It's
   ;; far too easy to accidentally bind over some `self-insert-command' binding
   ;; or even over \\[keyboard-quit].
   ("g" . nil)
   ("l" . nil))
  :config
  (setq embark-mixed-indicator-delay 2)

  ;; Make the eval action editable. Evaluating code in-place is simple enough
  ;; without Embark, if I invoke it with Embark, I almost definitely want to
  ;; edit the expression beforehand. And even if not, I can just confirm.
  (cl-pushnew 'embark--allow-edit
              (alist-get 'pp-eval-expression embark-target-injection-hooks))

  ;; Reload the project list after using
  ;; C-u `embark-act' with `project-forget-project'.
  (cl-pushnew 'embark--restart
              (alist-get 'project-forget-project embark-post-action-hooks))

  (defun embark-act-with-eval (expression)
    "Evaluate EXPRESSION and call `embark-act' on the result."
    (interactive "sExpression: ")
    (with-temp-buffer
      (let ((expr-value (eval (read expression))))
        (insert (if (stringp expr-value)
                    expr-value
                  (format "%S" expr-value))))
      (embark-act)))

  (dolist (keymap (list embark-variable-map embark-expression-map))
    (define-key keymap (kbd "v") #'embark-act-with-eval))

  ;; https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
  (autoload 'gnus-dired-attach "gnus-dired" nil t)
  (defun embark-attach-file (file)
    "Attach FILE to an email message."
    (interactive "fAttach: ")
    (cl-letf (((symbol-function 'y-or-n-p) #'always))
      (gnus-dired-attach (list file))))
  (bind-key "a" #'embark-attach-file embark-file-map))

(use-package embark-consult
  :after (embark consult))

(use-package marginalia
  :after vertico
  ;; :demand applies to :bind but not :after. We want to eagerly load marginalia
  ;; once vertico is loaded.
  :demand t
  :bind
  (:map minibuffer-local-map
        ("C-o" . marginalia-cycle))
  :config
  (marginalia-mode 1))

(use-package consult
  :bind
  (:map consult-mode-map
        ;; M-s
        ("M-s u" . consult-focus-lines)
        ("M-s k" . consult-keep-lines)
        ("M-s e" . consult-isearch-history)
        ("M-s d" . consult-find)
        ;; M-g
        ("M-g g" . consult-line)
        ("M-g o" . consult-outline)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-info)
        ("M-g r" . consult-ripgrep)
        ("M-g m" . consult-mark)
        ("M-g M" . consult-global-mark)
        ;; Misc
        ("C-x C-r" . consult-recent-file)
        ;; Remaps
        ([remap switch-to-buffer]              . consult-buffer)
        ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
        ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
        ([remap project-switch-to-buffer]      . consult-project-buffer)
        ([remap yank-pop]                      . consult-yank-pop)
        ([remap goto-line]                     . consult-goto-line)
        ([remap bookmark-jump]                 . consult-bookmark)
        ([remap repeat-complex-command]        . consult-complex-command)
        ;; Remaps for `Info-mode'.
        ([remap Info-search] . consult-info)

        :map isearch-mode-map
        ("TAB" . consult-line))
  :init
  (defvar consult-mode-map (make-sparse-keymap))
  (define-minor-mode consult-mode
    "Provide the `consult' commands in a single keymap."
    :global t
    (if consult-mode
        (define-key minibuffer-local-map
                    [remap previous-matching-history-element]
                    #'consult-history)
      (define-key minibuffer-local-map
                  [remap previous-matching-history-element]
                  nil)))
  (consult-mode 1)
  :config
  (consult-customize
   consult-ripgrep consult-grep
   consult-buffer consult-recent-file
   :preview-key "M-.")

  (defun atorres1985|orderless-fix-consult-tofu (pattern index total)
    "Ignore the last character which is hidden and used only internally."
    (when (string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                    "[\x200000-\x300000]*$"))))

  (dolist (command '(consult-buffer consult-line))
    (advice-add command :around
                (lambda (orig &rest args)
                  (let ((orderless-style-dispatchers (cons #'atorres1985|orderless-fix-consult-tofu
                                                           orderless-style-dispatchers)))
                    (apply orig args)))))

  ;; Disable consult-buffer project-related capabilities as they are very slow
  ;; in TRAMP.
  (setq consult-buffer-sources
        (delq 'consult--source-project-buffer
              (delq 'consult--source-project-file consult-buffer-sources)))

  (setq consult--source-hidden-buffer
        (plist-put consult--source-hidden-buffer :narrow ?h))

  (defvar atorres1985|consult--source-disassociated-file-buffer
    `(:name "Disassociated File"
            :narrow   ?e
            :category buffer
            :state    ,#'consult--buffer-state
            :items
            ,(lambda ()
               (consult--buffer-query
                :sort 'visibility
                :as #'buffer-name
                :predicate
                (lambda (buf)
                  (let ((file (atorres1985|buffer-file-or-directory-name buf)))
                    (and file (not (file-exists-p file)))))))
            "Disassociated buffer candidate source for `consult-buffer'.

Inspired by: `ibuffer-mark-dissociated-buffers'."))
  (defun atorres1985|consult-disassociated-buffers ()
    "Like `consult-buffer' but only for disassociated buffers."
    (interactive)
    (consult-buffer '(atorres1985|consult--source-disassociated-file-buffer)))

  (defvar atorres1985|consult--source-remote-file-buffer
    `(:name "Remote File"
            :narrow   ?r
            :hidden   t
            :category buffer
            :state    ,#'consult--buffer-state
            :items
            ,(lambda ()
               (consult--buffer-query
                :sort 'visibility
                :as #'buffer-name
                :predicate
                (lambda (buf)
                  (let ((file (atorres1985|buffer-file-or-directory-name buf)))
                    (and file (file-remote-p file))))))
            "Remote file buffer candidate source for `consult-buffer'."))
  (add-to-list 'consult-buffer-sources
               'atorres1985|consult--source-remote-file-buffer
               'append)

  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (add-to-list 'consult-bookmark-narrow
               '(?t "TMSU" tmsu-dired-bookmark-open)))

(use-package corfu
  :disabled
  :init
  (global-corfu-mode 1)
  :config
  (corfu-popupinfo-mode 1)
  (corfu-echo-mode 1)
  (setq corfu-popupinfo-delay '(nil . t)
        corfu-echo-delay t))

;;; https://archive.is/Gj6Fu
(autoload 'ffap-file-at-point "ffap")
(defun complete-path-at-point+ ()
  "As the name suggests."
  (let ((fn (ffap-file-at-point))
        (fap (thing-at-point 'filename)))
    (when (and (or fn (equal "/" fap))
               (save-excursion
                 (search-backward fap (line-beginning-position) t)))
      (list (match-beginning 0)
            (match-end 0)
            #'completion-file-name-table :exclusive 'no))))
(add-hook 'completion-at-point-functions
          #'complete-path-at-point+
          'append)

;;; Add prompt indicator to `completing-read-multiple'.
;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;;
;;; Taken from the Vertico docs.
(defun atorres1985|crm-indicator (args)
  "Add prompt indicator to `completing-read-multiple'.

We display [CRM: <separator>], e.g., [CRM: ,] if the separator is a comma.
The separator is given by the car of `ARGS'."
  (cons (format "[CRM: %s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'atorres1985|crm-indicator)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Use the completing-read UI for the M-tab completion unless
;;; overridden (for example by `corfu').
(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))

;;; END completion engine

(use-package dired-x
  :config
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..*$")))

;; Addtional syntax highlighting for dired
(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dired-sidebar
  :after dired
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode)))))

(use-package nhexl-mode)

(use-package dired
  :init
  ;; (setq dired-listing-switches "-ahil --group-directories-first")

  (defun atorres1985|dired-find-file-other-frame ()
    "In Dired, visit this file or directory in another window."
    (interactive)
    (find-file-other-frame (dired-get-file-for-visit)))

  (add-hook 'dired-load-hook
            (function (lambda ()
                        (load "dired-x"))))
  :config
  (define-key dired-mode-map "F" 'atorres1985|dired-find-file-other-frame))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/run/media/"                 "Drives")
     ("s" "/sources/"                   "Sources")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons
          collapse
          file-size
          file-time
          git-msg
          subtree-state
          vc-state))
  (setq dirvish-use-mode-line nil)
  (setq dirvish-use-header-line 'global)
  (setq dired-listing-switches
        "-l --all --inode --human-readable --group-directories-first --no-group")

  (dirvish-define-preview eza (file)
    "Use `eza' to generate directory preview."
    :require ("eza") ; tell Dirvish to check if we have the executable
    (when (file-directory-p file) ; we only interest in directories here
      `(shell . ("eza" "--all"
                 ;; "--blocksize"
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
                 ;; "--time-style=full-iso"
                 ,file))))

  (add-to-list 'dirvish-preview-dispatchers 'eza)

  (dirvish-define-preview elinks (file)
    "Use `elinks' to generate HTML preview."
    :require ("elinks")
    (when (string-match (rx (or ".htm" ".html") line-end) file) ; HTML
      `(shell . ("elinks"
                 "-dump"
                 "-localhost"
                 "-no-numbering"
                 "-no-references"
                 "-default-mime-type" "text/html"
                 "-dump-width" "80"
                 ,file))))

  (add-to-list 'dirvish-preview-dispatchers 'elinks)

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package eat)

(use-package eshell
  :config
  (defmacro with-face (string &rest face-properties)
    "A thin wrapper on propertize.

Return a copy of STRING with FACE-PROPERTIES added."
    `(propertize ,string 'face (list ,@face-properties)))

  (defun atorres1985|eshell-prompt ()
    "Custom eshell prompt."
    (let ((header-bg "#000"))
      (concat "\n"
              (with-face (concat (eshell/pwd) " ")
                         :background header-bg)
              (with-face (format-time-string "(%Y-%m-%d %H:%M) "
                                             (current-time))
                         :background header-bg
                         :foreground "#0000FF")
              (with-face
               (or (ignore-errors
                     (format "(%s)"
                             (vc-responsible-backend default-directory)))
                   "")
               :background header-bg)
              (with-face " " :background header-bg)
              (with-face "|" :background "grey")
              (with-face user-login-name
                         :foreground "blue")
              "@"
              (with-face "localhost"
                         :foreground "green")
              (if (= (user-uid) 0)
                  (with-face " #\n> "
                             :foreground "red")
                (with-face " $\n> "
                           :foreground "green")))))

  (setq eshell-prompt-function 'atorres1985|eshell-prompt)
  (setq eshell-highlight-prompt nil))

;;; --> misc <--

(defun atorres1985|sort-words (reverse begin end)
  "Sort words in region alphabetically, in REVERSE if negative.

Region is defined between BEGIN and END.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the
sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" begin end))

(defun atorres1985|sort-symbols (reverse begin end)
  "Sort symbols in region alphabetically, in REVERSE if negative.

Region is defined between BEGIN and END.

Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the
sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" begin end))

;; C language

(defun atorres1985|c-hook ()
  "Hook for C."
  (setq-default indent-tabs-mode nil)
  (setq c-basic-offset 4
        c-indent-level 4
        c-default-style "bsd"))

(dolist (hook '(smartparens-mode
                atorres1985|c-hook))
  (add-hook 'c-mode-common-hook hook))

;; Parenthesis

(use-package paren
  :config
  (show-paren-mode t)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'parenthesis))

(use-package text-mode
  :init
  (add-hook 'text-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'text-mode-hook #'turn-on-auto-fill)
  (add-hook 'text-mode-hook #'turn-on-flyspell))

(use-package display-fill-column-indicator
  :config
  (global-display-fill-column-indicator-mode))

(use-package bs
  :bind
  (("C-x C-b" . bs-show)))

(use-package helpful
  :bind
  (([remap describe-key] . helpful-key)
   ([remap describe-function] . helpful-callable)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-path (concat user-emacs-directory "share/tempel-templates.eld"))
  (tempel-trigger-prefix "<")

  :bind
  (("C-c t c" . tempel-complete) ;; Alternative tempel-expand
   ("C-c t i" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(use-package general
  :disabled
  :after
  ace-window
  bs
  consult
  dirvish
  dwim-shell-command
  dwim-shell-commands
  embark
  helpful
  marginalia
  tempel
  vertico
  ;;  :config
  )

(use-package dynamic-ruler
  :bind (("C-c r h" . dynamic-ruler)))

(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

(use-package emacs
  :init
  (when (display-graphic-p)
    (load-theme 'dracula t nil))

  ;; -- fonts
  (set-face-attribute 'default nil
                      :height 180)

  ;; frame configuration

  (setq initial-frame-alist (quote ((fullscreen . maximized))))
  (cl-pushnew '(width . 120) default-frame-alist) ; characters
  (cl-pushnew '(height . 36) default-frame-alist) ; lines

  (defmacro atorres1985|emit-backup-pair (extension directory)
    "Return a pair (regex . directory) suitable for use as `backup-directory-alist'.

The car of this pair is `EXTENSION' in regexp form, and the cdr is a
subdirectory of `DIRECTORY' named `EXTENSION'."

    `(cons (rx "." ,extension eol)
           (expand-file-name ,extension ,directory)))

  (defmacro atorres1985|expanded-form (form)
    "Output expanded form of given `FORM'."
    `(progn
       (pp (macroexpand-1 ',form))
       nil))

  (defun atorres1985|show-percentage ()
    "Calculate and display the percentage before and after the current point."
    (interactive)
    (let* ((frac (/ (* 1.0 (point)) (point-max)))
           (perc (* 100 frac))
           (rest (- 100 perc)))
      (message "atorres1985|show-percentage: %d + %d = %d"
               perc rest (+ perc rest))))

  (defun atorres1985|move-region (start end n)
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

  (defun atorres1985|move-region-up (start end n)
    "Move the region between START and END up by N lines."
    (interactive "r\np")
    (atorres1985|move-region start end (if (null n) -1 (- n))))

  (defun atorres1985|move-region-down (start end n)
    "Move the region between START and END down by N lines."
    (interactive "r\np")
    (atorres1985|move-region start end (if (null n) 1 n)))

  (defun atorres1985|toggle-fullscreen ()
    "Toggle fullscreen."
    (interactive)
    (set-frame-parameter nil 'fullscreen
                         (if (frame-parameter nil 'fullscreen)
                             nil
                           'fullboth)))

  (defun atorres1985|find-init-file ()
    "Open Emacs init file."
    (interactive)
    (find-file user-init-file))

  (defun atorres1985|load-directory (directory)
    "Load extra scripts scattered on DIRECTORY."
    (when (file-directory-p directory)
      (dolist (file (directory-files directory nil "el$"))
        (load-file (concat directory "/" file)))))

  (defun atorres1985|add-to-list-after (list-var old new &optional compare-fn)
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

  (defun atorres1985|buffer-file-or-directory-name (buf)
    "The file BUF is visiting, works also if it's a `dired' buffer."
    (with-current-buffer buf
      (or buffer-file-name
          (and (eq major-mode 'dired-mode)
               (boundp 'dired-directory)
               (file-name-directory
                (if (stringp dired-directory)
                    dired-directory
                  (car dired-directory)))))))

  (defun atorres1985|string-make-clickable (string click-command &optional help-echo)
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

  (defmacro atorres1985|add-to-local-keymap (&rest definitions)
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
  :config
  (defalias 'yes-or-no-p 'y-or-n-p
    "An alias for y-or-n-p to write less.")

  (setq-default fill-column 80
                frame-resize-pixelwise t
                indent-tabs-mode nil
                major-mode 'text-mode
                tab-width 4
                window-resize-pixelwise t
                word-wrap t)

  (setq auto-save-interval 300
        auto-save-timeout 30
        delete-by-moving-to-trash nil
        inhibit-startup-screen t        ; startup.el
        load-prefer-newer t
        visible-bell t)

  (setq frame-title-format
        '(buffer-file-name
          "%b - %f"                        ; File buffer
          (dired-directory dired-directory ; Dired buffer
                           (revert-buffer-function
                            "%b"                                 ; Buffer Menu
                            ("%b - Dir: " default-directory))))) ; Plain buffer

  (setq initial-scratch-message         ; defined at startup.el
        "\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This buffer is for notes you don't want to save, and to Emacs Lisp code. ;;;
;;; If you want to create a file, visit it with `M-x find-file' (usually     ;;;
;;; bounded to `C-x C-f'), then insert text in the file's own buffer.        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
")

  (let*
      ((backup-directory
        (expand-file-name ".text-backups" home-directory))
       (c-pair (atorres1985|emit-backup-pair "c" backup-directory))
       (el-pair (atorres1985|emit-backup-pair "el" backup-directory))
       (md-pair (atorres1985|emit-backup-pair "md" backup-directory))
       (nix-pair (atorres1985|emit-backup-pair "nix" backup-directory))
       (org-pair (atorres1985|emit-backup-pair "org" backup-directory))
       (py-pair (atorres1985|emit-backup-pair "py" backup-directory))
       (sh-pair (atorres1985|emit-backup-pair "sh" backup-directory))
       (tex-pair (atorres1985|emit-backup-pair "tex" backup-directory))
       (txt-pair (atorres1985|emit-backup-pair "txt" backup-directory))
       (adoc-pair (atorres1985|emit-backup-pair "adoc" backup-directory)))
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

  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (global-unset-key (kbd "C-x g"))
  (global-unset-key (kbd "<insert>"))

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file nil t))

  (atorres1985|load-directory (expand-file-name "extra-lisp.d" user-emacs-directory)))


;;; Always on bottom!
(use-package gcmh
  :config
  (setq gcmh-verbose nil)
  (gcmh-mode))

(provide 'init)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; End:

;;; init.el ends here
