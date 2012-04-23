;;; https://github.com/vedang/revive-mode


(define-key global-map "\C-cs" 'emacs-save-layout)
(define-key global-map "\C-cl" 'emacs-load-layout)
(add-hook 'kill-emacs-hook 'emacs-save-layout)








