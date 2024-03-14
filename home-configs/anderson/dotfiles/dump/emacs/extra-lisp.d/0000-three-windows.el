;;; -*- lexical-binding: t; no-byte-compile: t -*-

;; From https://www.emacswiki.org/emacs/ThreeWindows
;; https://spin.atomicobject.com/2016/05/27/write-emacs-package/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 ANTES                ;;;               DESPOIS             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------------------+   +-----------------+-----------------+
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   +-----------------+-----------------+
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; +-----------------------------------+   +-----------------+-----------------+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun three-windows-split-frame-into-4()
  "Split the frame into four sub-windows."
  (interactive)
  (when (= 1 (length (window-list)))
    (split-window-vertically)
	(split-window-horizontally)
	(other-window 2)
	(split-window-horizontally)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 ANTES                ;;;               DESPOIS             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------------------------+   +-----------------+-----------------+
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; +-----------------------------------+   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; +-----------------------------------+   +-----------------+-----------------+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun three-windows--change-split-type (split-function &optional arg)
  "Change 3 window style from horizontal to vertical and vice-versa"
  (let ((buffer-list (mapcar 'window-buffer (window-list))))
    (select-window (get-largest-window))
    (funcall split-function arg)
    (cl-mapcar 'set-window-buffer (window-list) buffer-list)))

(defun three-windows-transpose-vertical-horizontal (&optional arg)
  "Changes splitting from vertical to horizontal and vice-versa"
  (interactive "P")
  (let ((split-function (lambda (&optional arg)
                          (delete-other-windows-internal)
                          (if arg
                              (split-window-vertically)
                            (split-window-horizontally)))))
    (three-windows--change-split-type split-function arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 ANTES                ;;;               DESPOIS             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +-----------------+-----------------+   +-----------------+-----------------+
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |                                   |   |                 |                 |
;; |-----------------+-----------------|   |                 +-----------------+
;; |                 |                 |   |                 |                 |
;; |                 |                 |   |                 |                 |
;; |                 |                 |   |                 |                 |
;; +-----------------+-----------------+   +-----------------+-----------------+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun three-windows-change-split-type-3-horizontally (&optional arg)
  ""
  (interactive "P")
  (let ((split-function (lambda (&optional arg)
                          (delete-other-windows)
                          (split-window-vertically)
                          (unless arg
                            (other-window 1))
                          (split-window-horizontally))))
    (three-windows--change-split-type split-function arg)))

(defun three-windows-change-split-type-3-vertically (&optional arg)
  ""
  (interactive "P")
  (let ((split-function (lambda (&optional arg)
                          (delete-other-windows)
                          (split-window-horizontally)
                          (unless arg
                            (other-window 1))
                          (split-window-vertically))))
    (three-windows--change-split-type split-function arg)))
