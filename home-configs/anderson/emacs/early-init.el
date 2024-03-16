;;; early-init.el --- Emacs early init file -*- lexical-binding: t; no-byte-compile: nil -*-

;;; Commentary:
;;; Arquivo de Inicialização Prévia do Emacs
;;;

;;; Code:

(defconst atorres-1985|early-init-start-time (current-time)
  "Early init start time.")

(setopt package-archives nil
        package-enable-at-startup nil)

(mapc (lambda (mode)
        (when (fboundp mode)
          (apply mode '(-1))))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode))

(setopt frame-inhibit-implied-resize t
        frame-resize-pixelwise t)

(advice-add #'x-apply-session-resources :override #'ignore)

;; Removing the white flash
(when (display-graphic-p)
  (set-face-background 'default "#282a36" nil)
  (set-face-foreground 'default "#f8f8f2" nil))

(when (getenv-internal "EMACS_DEBUG")
  (setopt debug-on-error t)
  (setq-default init-file-debug t))

(setq backup-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist backup-file-name-handler-alist)
            (makunbound 'backup-file-name-handler-alist)))

(setopt gc-cons-threshold #x100000000
        gc-cons-percentage 0.9)

(defconst atorres-1985|early-init-finish-time (current-time)
  "Early init finish time.")

(provide 'early-init)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; fill-column: 80
;; End:

;;; early-init.el ends here
