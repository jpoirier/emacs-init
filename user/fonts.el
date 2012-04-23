;;; Fonts

;; From: http://community.schemewiki.org/cgi-bin/scheme.cgi?Emacs
;(and window-system
;     (create-fontset-from-fontset-spec
;      (concat
;       "-apple-monaco-medium-r-normal--12-*-*-*-*-*-fontset-monaco,"
;       "ascii:-apple-monaco-medium-r-normal--12-100-*-*-m-100-mac-roman"))
;     )

;(set-default-font "-apple-menlo-medium-r-normal--14-130-72-72-m-130-iso10646-1")



(if (eq system-type 'darwin)
	; something for OS X if true
  	; optional something if not
;  	(set-default-font "Menlo-14")
;        (set-frame-font "Menlo-14")
        (set-frame-font "Meslo LG M-14")
;        (set-frame-font "Meslo LG L-12")
;   (set-default-font "Menlo-11")
;     (set-frame-font "Meslo LG L-10")
     (set-frame-font "Meslo LG M-12")
)

;; To change a font size globally:
;; (set-face-attribute 'default nil :height 360)
;; (set-face-attribute 'default nil :height 240)
;; (set-face-attribute 'default nil :height 160)
;; (set-face-attribute 'default nil :height 120)

;; To change the font size interactively per buffer:
;; Up:    C-x C-+, C-x C-=
;; Down:  C-x C--
;; Reset: C-x C-0
