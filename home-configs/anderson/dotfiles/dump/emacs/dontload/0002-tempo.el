;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq tempo-interactive t)
(require 'tempo)

;; The following code provides a possibility to set the position of point after
;; expanding a tempo template. The position of point is indicated by '~':

(defvar tempo-initial-pos nil
  "Initial position in template after expansion.")

(defadvice tempo-insert (around tempo-insert-pos act)
  "Define initial position."
  (if (eq element '~)
      (setq tempo-initial-pos (point-marker))
    ad-do-it))

(defadvice tempo-insert-template (around tempo-insert-template-pos act)
  "Set initial position when defined."
  (setq tempo-initial-pos nil)
  ad-do-it
  (if tempo-initial-pos
      (progn
        (put template 'no-self-insert t)
        (goto-char tempo-initial-pos))
    (put template 'no-self-insert nil)))

(defun atorres1985|tempo-keyboard-macros ()
  ;;(local-set-key (read-kbd-macro "<f8>") 'tempo-forward-mark)
  (local-set-key (read-kbd-macro "C-<return>") 'tempo-complete-tag)
  (local-set-key (read-kbd-macro "<f5>") 'tempo-complete-tag))
