About
=====

ack-and-a-half.el provides a simple compilation mode for the perl
grep-a-like ack (http://petdance.com/ack/).

Installation
============

Add the following to your .emacs:

    (add-to-list 'load-path "/path/to/ack-and-a-half")
    (autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
    (autoload 'ack-and-a-half "ack-and-a-half" nil t)
    (autoload 'ack-and-a-half-find-file-samee "ack-and-a-half" nil t)
    (autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
    ;; Create shorter aliases
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

This will load the `ack-and-a-half` functions, and create shorter
aliases for them.

Credits
=======

ack-and-a-half was created from
[ack.el](http://rooijan.za.net/code/emacs-lisp/ack-el) and
[full-ack.el](http://nschum.de/src/emacs/full-ack/).  Both had
features that I liked, but neither was satisfactory on its own.  Thus
`ack-and-a-half` was born.
