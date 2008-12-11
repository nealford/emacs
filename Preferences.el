;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

;; frame transparency (1st value is focus window, 2nd is other)
(add-to-list 'default-frame-alist '(alpha . (85 75)))


;; Muse settings
(add-to-list 'load-path "/Users/nealford/bin/muse-3.12/lisp")

(require 'muse-mode)     ; load authoring mode

(require 'muse-html)     ; load publishing styles I use
(require 'muse-docbook)

(require 'muse-project)  ; publish files in projects


