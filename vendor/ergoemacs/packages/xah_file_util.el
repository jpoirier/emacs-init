;;; xah_file_util.el --- xah's misc elisp utility similar to unix grep/sed. -*- coding: utf-8 -*-

;; Copyright © 2012 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Created: 2012-04-02
;; Keywords: emacs lisp, utility, file

;; You can redistribute this program and/or modify it. Please give credit and link. Thanks.

;;; DESCRIPTION

;; this package is some misc emacs commands for find or find/replace on multiple files. It's similar in purpose to unix's {grep, sed}. It's entirely in emacs lisp.

;; The reason it's written because it avoids many problems when running emacs on Windows (e.g. no grep, lots problems interfacing with Windows ports of grep, unicode or encoding problems, emacs regex vs unix regex syntax mismatch for user, …, etc. See http://ergoemacs.org/emacs/emacs_grep_problem.html )

;; currently, this package is in alpha stage. I use it daily for months, but lots improvement in documentation and coding can be made.

;; This package exports the follow functions:
;; xah-find-text               → grep
;; xah-find-text-regex         → regex grep
;; xah-find-count              → grep count
;; xah-find-replace-text       → sed
;; xah-find-replace-text-regex → sed

;; Note: the commands here is not exactly modeled on unix grep.
;; in unix grep, it's based on lines. If a string happens twice in a line, that line will be reported only once (with the occurences highlighted).
;; the output of commands of this package is not based on lines.
;; there will be 50 chars showing before and after the searched text or pattern.
;; the number of chars to show is defined by `xah-context-char-number'
;; each “block of text” in output is one occurrence.
;; for example, if a line in a file has 2 occurrences, then the same line will be reported twice, as 2 “blocks”.
;; so, the number of blocks corresponds exactly to the number of occurrences.

;; donate $3 please. Paypal to xah@xahlee.org , thanks.

;;; INSTALL

;;; HISTORY

;; version 1.6.8, 2013-07-05 • More options added to “xah-find-text”, “xah-find-text-regex”, “xah-find-replace-text”. Output format improved. Much code refactoring.
;; version 1.6.7, 2013-06-17 • WARNING the argument for case search is reversed for xah-find-replace-text-regex. • added a case search option for xah-find-text-regex
;; version 1.6.6, 2012-12-16 Now, the backup file's suffix is same for all backup files created during one command call. Before, each backup file has timestamp when the backup file is created, that is, their seconds will differ.
;; version 1.6.5, 2012-12-08 improved the prompt for “xah-find-count” and also its output.
;; version 1.6.4, 2012-12-06 Backup file name now has this format: 「~‹x›~‹datetimestamp›~」 where ‹x› is 「t」 for plain text replace and 「r」 for regex replace. e.g. 「x.html~r~20121206_095642~」 Also, modified the prompt for 「xah-find-replace-text-regex」 so it is consistent with the function's argument.
;; version 1.6.3, 2012-11-30 fixed a bug: when one of the find or find/replace is called, and the temp output buffer already exits, the highlighting doesn't work. Now it does work.
;; version 1.6.2, 2012-11-29 trival change. Changed output file names to consistently start with “•” instead of some “◆”
;; version 1.6.1, 2012-11-20 improved the highlighting for xah-find-replace-text. It now highlighting the replaced text, instead of the find text.
;; version 1.6, 2012-08-12 added xah-find-count.
;; version 1.5, 2012-07-24 minor modification to the output format, made more consistent, added a utf-8 header.
;; version 1.4, 2012-07-21 added prompt for a dir on “xah-find-text” and all others.
;; version 1.3, 2012-07-19 added “xah-find-replace-text-regex”
;; version 1.2, 2012-07-14 added “xah-find-replace-text”
;; version 1.1, 2012-05-11 modified xah-find-text so that same line are not printed.
;; version 1.0, 2012-04-02 First version.


;;; Code:

(require 'find-lisp)
(require 'xeu_elisp_util)

(defcustom xah-context-char-number 50 "number of characters to print before and after a search string."
:group 'xah_file_util
)

(defun xah-backup-suffix (εs)
  "Return a string of the form 「~‹s›~‹date-time-stamp›~」"
  (concat "~" εs "~" (format-time-string "%Y%m%d_%H%M%S") "~"))

(defun xah-print-text-block (εstring9462)
  "print string9462"
  (princ (format "「%s」
" εstring9462)))

(defun xah-print-file-count (εfilepath4287 εcount8086)
  "print file path and count"
  (princ (format "• %d %s\n" εcount8086 εfilepath4287 ))
  )

(defun xah-find-text (εsearch-str1 εinput-dir εpath-regex εfixed-case-search-p ξprintContext-p)
  "Report files that contain string, similar to Linux 「grep -F」."
  (interactive
   (list
    (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Fixed case in search?")
    (y-or-n-p "Print surrounding Text?")
 ) )

  (let (
        (case-fold-search (not εfixed-case-search-p))
        (ξcount 0)
        (ξoutputBuffer "*xah-find-text output*")
        p1 ; context begin position
        p2 ; context end position
        )

    (setq εinput-dir (file-name-as-directory εinput-dir) ) ; normalize dir path

    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-text result.
Search string 「%s」
Directory 「%s」
Path Regex 「%s」

" (current-date-time-string) εsearch-str1 εinput-dir εpath-regex))
      (mapc
       (lambda (ξfp)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents ξfp)
           (while (search-forward εsearch-str1 nil "NOERROR if not found")
             (setq ξcount (1+ ξcount))
             (setq p1 (max 1 (- (match-beginning 0) xah-context-char-number )) )
             (setq p2 (min (point-max) (+ (match-end 0) xah-context-char-number )) )
             (when ξprintContext-p (xah-print-text-block (buffer-substring-no-properties p1 p2 )) ))
           (when (> ξcount 0)
             (xah-print-file-count ξfp ξcount)
              ) ) )
       (find-lisp-find-files εinput-dir εpath-regex))

      (switch-to-buffer ξoutputBuffer)
      (hi-lock-mode 0)
      (funcall 'fundamental-mode)
      (highlight-phrase (regexp-quote εsearch-str1) (quote hi-yellow))
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      ) ))

(defun xah-find-text-regex (εsearch-regex εinput-dir εpath-regex εfixed-case-search-p ξprintContextLevel )
  "Report files that contain a string pattern, similar to unix grep."
  (interactive
   (list
    (read-string (format "Search regex (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Fixed case search?")
    (ido-completing-read "Print context level (0=none, 1=matched pattern, 2=neighboring string) " '("0" "1" "2"))
    )
   )

  (let (
        (ξcount 0)
        (ξoutputBuffer "*xah-find-text-regex output*")
        (ξpos1 1) ; beginning of line
        (ξpos2 1)
        )

    (setq εinput-dir (file-name-as-directory εinput-dir) ) ; add ending slash

    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-text-regex result.
Search regex 「%s」
Directory 「%s」
Path Regex 「%s」

" (current-date-time-string) εsearch-regex εinput-dir εpath-regex))
      (mapc
       (lambda (ξfp)
         (setq ξcount 0)
         (with-temp-buffer
           (insert-file-contents ξfp)
           (setq case-fold-search (not εfixed-case-search-p))
           (while (search-forward-regexp εsearch-regex nil t)
             (setq ξcount (1+ ξcount))
             (cond
              ((equal ξprintContextLevel "0") nil)
              ((equal ξprintContextLevel "1") (xah-print-text-block (match-string 0)))
              ((equal ξprintContextLevel "2")
               (progn
                 (setq ξpos1 (max 1 (- (match-beginning 0) xah-context-char-number )))
                 (setq ξpos2 (min (point-max) (+ (match-end 0) xah-context-char-number )))
                 (xah-print-text-block (buffer-substring-no-properties ξpos1 ξpos2 ))))
              )
             )
           (when (> ξcount 0)
             (xah-print-file-count ξfp ξcount)
             )
           )
         )
       (find-lisp-find-files εinput-dir εpath-regex))

      (switch-to-buffer ξoutputBuffer)
      (hi-lock-mode 0)
      (funcall 'fundamental-mode)
      (highlight-phrase εsearch-regex (quote hi-yellow))
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    ))

(defun xah-find-replace-text (εsearch-str εreplace-str εinput-dir εpath-regex εfixed-case-search-p εfixed-case-replace-p)
  "Find/Replace string in all files of a directory.
SearchStr can span multiple lines.
No regex."
  (interactive
   (list
    (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Fixed case in search?")
    (y-or-n-p "Fixed case in replacement?")
    )
   )

  (let (
        (ξoutputBuffer "*xah-find-replace-text output*")
        (backupSuffix (xah-backup-suffix "t"))
        )
    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-replace-text result.
Search string 「%s」
Replace string 『%s』
Directory 〔%s〕

" (current-date-time-string) εsearch-str εreplace-str εinput-dir))
      (mapc
       (lambda (ξf)
         (let ( (case-fold-search (not εfixed-case-search-p))
                (ξcount 0)
                )
           (with-temp-buffer
             (insert-file-contents ξf)
             (while (search-forward εsearch-str nil t)
               (replace-match εreplace-str εfixed-case-replace-p "literalreplace")
               (setq ξcount (1+ ξcount) )
               (xah-print-text-block (buffer-substring-no-properties
                                    (max 1 (- (match-beginning 0) xah-context-char-number ))
                                    (min (point-max) (+ (point) xah-context-char-number ))))
               )

             (when (> ξcount 0)
               (copy-file ξf (concat ξf backupSuffix) t)
               (write-region 1 (point-max) ξf)
               (xah-print-file-count ξf ξcount )
               ) )
           ))

       (find-lisp-find-files εinput-dir εpath-regex))
      (princ "Done.")
      )
    (switch-to-buffer ξoutputBuffer)
    (hi-lock-mode 0)
    (funcall 'fundamental-mode)
    (progn
      (when (not (string= εreplace-str ""))
        (highlight-phrase (regexp-quote εreplace-str) (quote hi-yellow))
        )
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    )
  )

(defun xah-find-replace-text-regex (ξregex εreplace-str εinput-dir εpath-regex ξwriteToFile-p εfixed-case-search-p εfixed-case-replace-p)
  "Find/Replace by regex in all files of a directory.

ξregex is a regex pattern.
εreplace-str is replacement string.
εinput-dir is input directory to search (includes all nested subdirectories).
εpath-regex is a regex to filter file paths.
ξwriteToFile-p, when true, write to file, else, print a report of changes only.
ξcaseFoldSearch-p sets `case-fold-search' for this operation.
εfixed-case-replace-p, if true, then the letter-case in replacement is literal. (this is relevant only if ξcaseFoldSearch-p is true.)
"
  (interactive
   (list
    (read-regexp "regex: " )
    (read-string (format "Replace string: ") nil 'query-replace-history)
    (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
    (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
    (y-or-n-p "Write changes to file?")
    (y-or-n-p "Fixed case in search?")
    (y-or-n-p "Fixed case in replacement?")
    )
   )

  (let (
        (ξoutputBuffer "*xah-find-replace-text-regex output*")
        (backupSuffix (xah-backup-suffix "r"))
        )
    (with-output-to-temp-buffer ξoutputBuffer
      (princ (format "-*- coding: utf-8 -*-
%s
xah-find-replace-text-regex result.
Search string 「%s」
Replace with 『%s』
Directory 〔%s〕

" (current-date-time-string) ξregex εreplace-str εinput-dir))
      (mapc
       (lambda (ξfp)
         (let (
                (ξcount 0)
                matchStrFound matchStrReplaced )

           (when t
             (with-temp-buffer
               (insert-file-contents ξfp)
               (setq case-fold-search (not εfixed-case-search-p))
               (while (re-search-forward ξregex nil t)
                 (setq matchStrFound (match-string 0))
                 (replace-match εreplace-str εfixed-case-replace-p)
                 (setq matchStrReplaced (match-string 0))
                 (setq ξcount (1+ ξcount) )
                 (princ (format "「%s」\n" matchStrFound))
                 (princ (format "『%s』\n" matchStrReplaced))
                 )

               (when (> ξcount 0)
                 (when ξwriteToFile-p
                   (copy-file ξfp (concat ξfp backupSuffix) t)
                   (write-region 1 (point-max) ξfp)
                   )
                 (princ (format "• %d %s\n" ξcount ξfp))
                 ) )
             )

           ))

       (find-lisp-find-files εinput-dir "\\.html$"))
      (princ "Done ☺")
      )

    (switch-to-buffer ξoutputBuffer)
    (hi-lock-mode 0)
    (funcall 'fundamental-mode)
    (progn
      (when (not (string= εreplace-str ""))
        (highlight-phrase (regexp-quote ξregex) (quote hi-yellow))
        )
      (highlight-lines-matching-regexp "^• " (quote hi-pink))
      )
    )
  )

(defun xah-find-count (εsearch-str ξcountExpr ξcountNumber εinput-dir εpath-regex)
  "Report how many occurances of a string, of a given dir.
Similar to grep, written in elisp.

Case sensitivity is determined by `case-fold-search'. Call `toggle-case-fold-search' to change."
  (interactive
   (let* ( ξoperator)
     (list
      (read-string (format "Search string (default %s): " (current-word)) nil 'query-replace-history (current-word))
      (setq ξoperator (ido-completing-read "Greater less equal unqual:" '("<" ">" "<=" ">=" "=" "/=")) )
      (read-string (format "Count %s: "  ξoperator) "0")
      (read-directory-name "Directory: " default-directory default-directory "MUSTMATCH")
      (read-from-minibuffer "Path regex: " nil nil nil 'dired-regexp-history)
      ))
   )

  (let* (
         (outputBuffer "*xah-find-count output*")
         (countOperator
          (cond
           ((string-equal "<" ξcountExpr ) '<)
           ((string-equal "<=" ξcountExpr ) '<=)
           ((string-equal ">" ξcountExpr ) '>)
           ((string-equal ">=" ξcountExpr ) '>=)
           ((string-equal "=" ξcountExpr ) '=)
           ((string-equal "/=" ξcountExpr ) '/=)
           (t (error "your count expression 「%s」 is wrong!" ξcountExpr ))
           )
          )
         (countNumber (string-to-number ξcountNumber))
         )

    (with-output-to-temp-buffer outputBuffer
(princ (format "-*- coding: utf-8 -*-
Date: %s
Command “xah-find-count” result.
Search string: 「%s」
Count expression: 「%s %s」
Input dir: 「%s」
Path regex: 「%s」
" (current-date-time-string) εsearch-str ξcountExpr ξcountNumber εinput-dir εpath-regex))
      (mapc
       (lambda (ξf)
         (let ((ξcount 0)
               )
           (when t
             (with-temp-buffer
               (insert-file-contents ξf)
               (goto-char 1)
               (while (search-forward εsearch-str nil "NOERROR if not found")
                 ;; (princ (format "「%s」\n" (buffer-substring-no-properties (line-beginning-position) (line-end-position) )))
                 (setq ξcount (1+ ξcount))
                 )

               ;; report if the occurance is not n times
               (when
                   (funcall countOperator ξcount countNumber)
                 (princ (format "• %d %s\n" ξcount ξf))
                 )
               )
             )

           )
         )
       (find-lisp-find-files εinput-dir "\\.html$"))
      (princ "Done deal!")
      )

    (switch-to-buffer outputBuffer)
    (hi-lock-mode 0)
    (funcall 'fundamental-mode)
    (highlight-phrase εsearch-str (quote hi-yellow))
    (highlight-lines-matching-regexp "^• " (quote hi-pink))

    ))

(provide 'xah_file_util)
