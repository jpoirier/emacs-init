This emacs folder sits in my AeroFS folder (Linux/OSX/Windows) with the
following in my .emacs file:

.emacs
------
```
(add-to-list 'load-path “~/AeroFS/emacs-init")
(load "init")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(ergoemacs-mode-used "5.8.0")
 '(ergoemacs-theme nil)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(kill-ring-max 600)
 '(py-pychecker-command "pychecker.sh")
 '(py-pychecker-command-args (quote ("")))
 '(python-check-command "pychecker.sh")
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))
```

init.el
-------
- does add-to-list and load-path for stuff in the vendor folder
- loads stuff from the user folder

user folder
-----------
- contains system specific initialization files, e.g. bindings.el, require.el, global.el, env.el
- contains files for setting/configuring items in the vendor folder

vendor folder
-------------
- some add-on functionality


I don't have Dropbox running on all the machines I work on, e.g. lab computers at work, so I keep a copy on Github for easy access.
