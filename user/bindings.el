;;; Global key bindigns
;;
;; How to Define Keyboard Shortcuts in Emacs
;; http://xahlee.org/emacs/keyboard_shortcuts.html
;;
;; On OSX:
;;   - Use keyboard prefs to switch the caps lock key with the control key (External & MBP both)
;;   - Assign the meta key to the command key
;;   - Assign the alt key to the option key
;;
;; On MSW, windows keyboard:
;;   - Use remapkey.exe, from the windows resource kit, to switch the caps lock key with the control key
;;   - Use remapkey.exe, from the windows resource kit, to switch the alt key with the windows key
;;   - De-map the alt as meta key
;;   - Set the left-windows key as the meta key
;; 
;; On MSW, apple keyboard:
;;   - Use remapkey.exe, from windows resource kit, to switch the caps lock key with the control key
;;   - Apr2012 Don't change - De-map the alt as meta key
;;   - Apr2012 Don't change - Set the left-windows key as the meta key
;; 


(if (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil 
        mac-command-key-is-meta t 
        mac-command-modifier 'meta 
        mac-option-modifier `alt)
)

;(if (eq system-type 'windows-nt)
;    (setq w32-alt-is-meta 'nil
;      w32-lwindow-modifier 'meta
;    )   
;)

;;; Ace Jump
;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;(define-key global-map (kbd "C-u C-u SPC") 'ace-jump-char-mode)
;(define-key global-map (kbd "C-u C-u C-c SPC") 'ace-jump-line-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "C-c c c SPC") 'ace-jump-line-mode)

(global-set-key (kbd "A-v") 'scroll-down)
(global-set-key (kbd "A-c") 'capitalize-word)

;;; CUA
(setq cua-enable-cua-keys t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

(setq mouse-drag-copy-region nil)   ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection

;;; Tab bar and tab navigation settings
(tabbar-mode)
(global-set-key (kbd "<f1>") 'tabbar-backward)
(global-set-key (kbd "<f2>") 'tabbar-forward)
(setq tabbar-cycling-scope "tabs")
(global-set-key (kbd "<f3>") 'sr-speedbar-toggle)
(global-set-key (kbd "<f4>") 'sr-speedbar-select-window)
(global-set-key (kbd "<f5>") 'delete-trailing-whitespace)

;;; Window nav: M->, M<-, M/\, M\/
(windmove-default-keybindings 'meta)

;;; Map the window manipulation keys to meta 0, 1, 2, o
(global-set-key (kbd "M-3") 'split-window-horizontally)		; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) 		; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) 		    ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) 			    ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) 			        ; was facemenu-keymap

;;;
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-m") 'set-mark-command)

;; Replace dired's M-o
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode

;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window

;; ibuffer > list-buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Easier buffer killing
;(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)

;; tabs.el
(global-set-key (kbd "TAB") 'smart-tab)

;; rectangle.el
(global-set-key (kbd "C-x r M-k") 'kill-save-rectangle)

;; expand-region.el
(global-set-key (kbd "C-=") 'er/expand-region)

;; Find matching parens/bracket
(global-set-key (kbd "C-'") 'match-paren)

;;;----------------------------------------------------
;;;----------------------------------------------------

;;; 26May2010: need to remap:
;; M-c Capitalize first letter of word
;; M-v Move down one screen
;; M-z Zap char

;(global-set-key "\C-w" 'backward-kill-word)
;(global-set-key "\C-x\C-k" 'kill-region)
;;(global-set-key "\C-c\C-k" 'kill-region)
;(global-set-key (kbd "M-s") 'save-buffer)
;;(global-set-key (kbd "M-x") 'clipboard-kill-region)
;;(global-set-key (kbd "C-x") 'clipboard-kill-region)
;(global-set-key (kbd "A-x") 'clipboard-kill-region)
;(global-set-key (kbd "M-c") 'clipboard-kill-ring-save)
;(global-set-key (kbd "M-v") 'clipboard-yank)
;(global-set-key (kbd "M-z") 'undo)
;(global-set-key (kbd "M-y") 'redo)
;;(global-set-key (kbd "M-y") 'repeat) ; was suspend-frame

;; workgroup.el
;(setq wg-prefix-key (kbd "A-w"))
;(setq wg-prefix-key (kbd "C-c w"))

;;
;(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
;(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
;(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
;(global-unset-key (kbd "C-x 0")) ; was delete-window
;(global-unset-key (kbd "C-x o")) ; was other-window

;(global-set-key (kbd "C-c <up>") 'windmove-up)
;(global-set-key (kbd "C-c <down>") 'windmove-down)
;(global-set-key (kbd "C-c <left>") 'windmove-left)
;(global-set-key (kbd "C-c <right>") 'windmove-right)

;(define-key global-map "\C-cp" 'windmove-up)
;(define-key global-map "\C-cn" 'windmove-down)
;(define-key global-map "\C-cb" 'windmove-left)
;(define-key global-map "\C-cf" 'windmove-right)

;w32-register-hot-key
;w32-pass-lwindow-to-system and w32-pass-rwindow-to-system nil
;hyper, super, meta, alt, control, or shift
;(setq mac-command-key-is-meta t)
;(setq mac-command-modifier 'meta)

; ns-control-modifier: control
; ns-command-modifier: meta
; ns-alternate-modifier: 'meta
;(setq mac-command-key-is-meta nil)

;; Window manipulation
;(global-set-key [(control kp-6)] 'enlarge-window-horizontally)
;(global-set-key [(control kp-4)] 'shrink-window-horizontally)
;(global-set-key [(control kp-8)] 'enlarge-window)
;(global-set-key [(control kp-2)] 'shrink-window)

;; Find stuff
;(global-set-key [(f2)]              'ack-default-directory)
;(global-set-key [(control f2)]      'ack-same)
;(global-set-key [(control meta f2)] 'ack)
;(global-set-key [(meta f2)]         'find-name-dired)
;(global-set-key [(shift f2)]        'occur)

;; Keyboard macros
;(global-set-key [(shift f4)] 'kmacro-start-macro-or-insert-counter)
;;(global-set-key [(f4)]    'kmacro-end-or-call-macro)  ;; already defined

;; Refresh-like
;(global-set-key [(f5)]         'revert-buffer)
;(global-set-key [(control f5)] 'toggle-read-only)

;; Indenting and alignment
;(global-set-key [(f8)]         'indent-region)
;(global-set-key [(control f8)] 'align)
;(global-set-key [(shift f8)]   'align-current)
;(global-set-key [(meta f8)]    'align-regexp)

;; Version control and change related
;(global-set-key [(control f9)] (lambda () (interactive) (magit-status default-directory)))
;(global-set-key [(f9)]         (lambda () (interactive) (magit-status default-directory)))
;(global-set-key [(meta f9)]    'autotest-switch)  ;; Move to ruby/rails mode?

;; Mac OS X conventions
;(global-set-key (kbd "M-a") 'mark-whole-buffer) ; was backward-sentence.

;; Easy inserts
;(global-set-key (kbd "C-.") 'insert-arrow)

;; Improved navigation and editing (assumes misc.el)
;(global-set-key (kbd "M-Z") 'zap-up-to-char)
;(global-set-key (kbd "M-F") 'forward-to-word)
;(global-set-key (kbd "M-B") 'backward-to-word)

;;; Bind function to key for whitespace removal
;(global-set-key (kbd "<f3>") 'delete-trailing-whitespace)
;(global-set-key (kbd "<f4>") 'auto-complete-activate)

;; zoom.el
;(global-set-key [(meta \))] 'zoom-way-out)
;(global-set-key [(meta _)] 'zoom-in)
;(global-set-key [(meta +)] 'zoom-out)

;; orphan.el
;(global-set-key [C-tab] 'next-multiframe-window)
;(global-set-key (kbd "C-c t") 'toggle-transparency)
;(global-set-key (kbd "C-c s") 'swap-windows)

;; maxframe.el
;(global-set-key [(meta return)] 'mf)

;; fullscreen.el
;(global-set-key [f6] 'toggle-fullscreen)




