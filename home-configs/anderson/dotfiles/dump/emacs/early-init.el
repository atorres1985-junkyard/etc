;;; early-init.el --- Emacs early init file -*- lexical-binding: t; no-byte-compile: nil -*-

;;; Commentary:
;;; Emacs early init file
;;;

;;; Code:

;; Package initialization is done via nixpkgs; we must prevent Emacs from doing
;; it early!

(setq package-archives nil
      package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;; Prevent X session resources to be loaded by Emacs
(advice-add #'x-apply-session-resources :override #'ignore)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

(provide 'early-init)

;;; early-init.el ends here
