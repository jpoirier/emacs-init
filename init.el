;; init.el should be loded from .emacs file.
;; Note, vendor load paths are OS specific.
;; User loads happen last and in 3 stages. 

;; M-x set-variable
;; debug-on-error
;; t
;; M-x load-file
;; ~/.emacs

;; Enable a backtrace when problems occur
;(setq debug-on-error t)

(if (eq system-type 'windows-nt)
        (setq path-string-vendor "C:/Users/jpoirier/Dropbox/emacs-init/vendor")
    (setq path-string-vendor "~/Dropbox/emacs-init/vendor"))

(add-to-list 'load-path (concat path-string-vendor "/ace-jump-mode"))
(add-to-list 'load-path (concat path-string-vendor "/ack-and-a-half"))
(add-to-list 'load-path (concat path-string-vendor "/auto-complete"))
;(add-to-list 'load-path (concat path-string-vendor "/auto-indent-mode"))
;(add-to-list 'load-path (concat path-string-vendor "/color-theme-solarized"))
(add-to-list 'load-path (concat path-string-vendor "/color-theme"))
(add-to-list 'load-path (concat path-string-vendor "/company"))
(add-to-list 'load-path (concat path-string-vendor "/expand-region"))
(add-to-list 'load-path (concat path-string-vendor "/go-mode"))
;(add-to-list 'load-path (concat path-string-vendor "/layout-restore"))
(add-to-list 'load-path (concat path-string-vendor "/magit"))
;(add-to-list 'load-path (concat path-string-vendor "/package"))
;(add-to-list 'load-path (concat path-string-vendor "/pastels-on-dark-theme"))
(add-to-list 'load-path (concat path-string-vendor "/redo+"))
(add-to-list 'load-path (concat path-string-vendor "/revive-mode"))
(add-to-list 'load-path (concat path-string-vendor "/sr-speedbar"))
(add-to-list 'load-path (concat path-string-vendor "/sunrise-commander"))
(add-to-list 'load-path (concat path-string-vendor "/tabbar"))
(add-to-list 'load-path (concat path-string-vendor "/tramp"))
;(add-to-list 'Info-default-directory-list (concat path-string-vendor "/tramp/info/"))
;(add-to-list 'load-path (concat path-string-vendor "/workgroups"))
(add-to-list 'load-path path-string-vendor)

;; 1. Prologue, order dependent
(load "user/env")
(load "user/require")
(load "user/defuncs")
(load "user/color-theme")
;(load "user/color-theme-solarized")
(load "user/global")
;(load "user/workgroups")
(load "user/revive-mode")

;; 2. Intermediate
(load "user/tabs")
(load "user/disabled")
(load "user/fonts")
(load "user/utf-8")
(load "user/scratch")
(load "user/grep")
(load "user/diff")
(load "user/ido")
(load "user/dired")
(load "user/recentf")
(load "user/rectangle")
(load "user/zoom")
;(load "user/fullscreen")

(if (eq system-type 'darwin)
    (load "user/mac"))

(load "user/hl-line")
(load "user/orphan")
(load "user/ack-and-a-half")
(load "user/magit")
(load "user/speedbar")
;(load "user/package")
;(load "user/fullscreen")

;; 3. Epilogue
(load "user/bindings")
(load "user/custom-vars")
;(load "user/ac")
(load "user/company")
(load "user/expand-region")
;(load "user/auto-indent-mode")
;(load "user/layout-restore")
