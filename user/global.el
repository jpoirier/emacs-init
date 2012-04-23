;;; Generic emacs settings I cannot live without

(setq-default initial-scratch-message nil)
;(setq initial-scratch-message "?_?") 

(desktop-save-mode 1)
(tool-bar-mode -1)


;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory 
	 			  dired-directory
				  (revert-buffer-function " %b"
				  ("%b - Dir:  " default-directory)))))))
;; Smart tab
(setq tab-always-indent 'complete)

;; Use command as the meta key
;(setq ns-command-modifier (quote meta))

;(setq cua-enable-cursor-indications nil)
;(set-cursor-color "yellow")
(setq-default cursor-type 'bar) 

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; Emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Explicitly show the end of a buffer
(set-default 'indicate-empty-lines t) 

;; Line-wrapping
;(set-default 'fill-column 80)
;(setq auto-fill-mode 0)
;(setq-default word-wrap nil)
;(setq longlines-wrap-follows-window-size nil)

;; Prevent the annoying beep on errors
;; (setq visible-bell t)

;; Make sure all backup files only live in one place
;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Gotta see matching parens
(show-paren-mode t)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; For emacsclient
(server-start)

;; Trailing whitespace is unnecessary
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; `brew install aspell --lang=en` (instead of ispell)
;(setq-default ispell-program-name "aspell")
;(setq ispell-list-command "list")
;(setq ispell-extra-args '("--sug-mode=ultra"))


;; --------------------------------------------------------
;; nice little alternative visual bell; Miles Bader <miles /at/ gnu.org>
(defcustom echo-area-bell-string "*DING* " ;"?"
 "Message displayed in mode-line by `echo-area-bell' function."
 :group 'user)
(defcustom echo-area-bell-delay 0.1
 "Number of seconds `echo-area-bell' displays its message."
 :group 'user)
;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)
(defun echo-area-bell ()
 "Briefly display a highlighted message in the echo-area.
The string displayed is the value of `echo-area-bell-string',
with a red background; the background highlighting extends to the
right margin.  The string is displayed for `echo-area-bell-delay'
seconds.
This function is intended to be used as a value of `ring-bell-function'."
 (unless (equal echo-area-bell-string echo-area-bell-cached-string)
   (setq echo-area-bell-propertized-string
         (propertize
          (concat
           (propertize
            "x"
            'display
            `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
           echo-area-bell-string)
          'face '(:background "yellow")))
   (setq echo-area-bell-cached-string echo-area-bell-string))
 (message echo-area-bell-propertized-string)
 (sit-for echo-area-bell-delay)
 (message ""))
(setq ring-bell-function 'echo-area-bell) 
;; --------------------------------------------------------


