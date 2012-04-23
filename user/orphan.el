;;; Find a place for these

;(set-frame-position (selected-frame) 80 5)
;(set-frame-size (selected-frame) 135 40)

(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;;;
(setq visible-bell t)                          ; visible bell but no sound
;(setq ring-bell-function 'ignore)              ; Not visible and no sound
(setq require-final-newline 't)                ; always add a newline at end of file
(setq tab-width 4)                             ; 4 spaces per tab
(delete-selection-mode 1)                      ; delete selected region
(defun delete-active-region (&optional killp)
  (delete-region (point) (mark))
  t)
(setq make-backup-files nil)                   ; stop creating those backup~ files
(setq auto-save-default nil)                   ; stop creating those #auto-save# files

;;; Code style settings
(setq c-default-style "bsd")
(setq c-indent-level 4)
(setq c-tab-width 4)

;(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100))
;(normal-erase-is-backspace-mode 1)             ; correct delete key behavior

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;(load-file "~/Dropbox/emacs/cedet/common/cedet.el")

;(eval-when-compile (require 'cl))
; (defun toggle-transparency ()
;   (interactive)
;   (if (/=
;        (cadr (find 'alpha (frame-parameters nil) :key #'car))
;        100)
;       (set-frame-parameter nil 'alpha '(100 100))
;     (set-frame-parameter nil 'alpha '(85 60))))

;(defun swap-windows ()
;  (interactive)
;  (cond ((/= (count-windows ) 2)
;         (message "you need 2 windows"))
;        (t
;         (let* ((w1 (first (window-list)))
;                (w2 (second (window-list)))
;                (b1 (window-buffer w1))
;                (b2 (window-buffer w2))
;                (s1 (window-start w1))
;                (s2 (window-start w2)))
;           (set-window-buffer w1 b2)
;           (set-window-buffer w2 b1)
;           (set-window-start w1 s2)
;           (set-window-start w2 s1))))
;  (other-window 1))








