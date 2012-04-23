;;; sr-speedbar additions

(speedbar-add-supported-extension ".go")
;   (add-to-list 'speedbar-fetch-etags-parse-list
;		'("\\.go" . speedbar-parse-c-or-c++tag))


(speedbar-add-supported-extension ".js")
;   (add-to-list 'speedbar-fetch-etags-parse-list
;		'("\\.js" . speedbar-parse-c-or-c++tag))


;;; https://github.com/nixme/.emacs.d/blob/master/init-speedbar.el
;; show all files
(setq speedbar-show-unknown-files t)

;; turn off the ugly icons
(setq speedbar-use-images nil)

;; left-side pane
(setq sr-speedbar-right-side nil)

;; don't refresh on buffer changes
(setq sr-speedbar-auto-refresh t)

;; disable line numbers in the speedbar frame
;(add-to-list 'linum-disabled-modes-list '(speedbar-mode))

;; nicer fonts for speedbar when in GUI
(when (window-system)
  ;; keep monospace buttons, but smaller height
  (set-face-attribute 'speedbar-button-face nil :height 100)

;; change to system default UI font for entries
(dolist (face (list 'speedbar-file-face 'speedbar-directory-face
                      'speedbar-tag-face  'speedbar-selected-face
                      'speedbar-highlight-face))
    (if (eq system-type 'windows-nt)
            (set-face-attribute face nil :family "Meslo LG S" :height 100)
;            (set-face-attribute face nil :family "Droid Sans" :height 100)
        (set-face-attribute face nil :family "Meslo LG S" :height 120))))
;        (set-face-attribute face nil :family "Lucida Grande" :height 110))))

;; no left fringe and half-size right fringe. TODO: doesn't work
;(add-to-list 'speedbar-frame-parameters '(left-fringe . 0))

;(provide 'init-speedbar)
