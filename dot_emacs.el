;; setup load path
(setq load-path (cons "~/work/emacs" load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; language modes


;;;;;;;;;;;;;;;;
;; scala
(add-to-list 'load-path "~/work/emacs/scala")  
(require 'scala-mode-auto)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customizations

;; set up alt key to work as META on Mac
(set-keyboard-coding-system 'mac-roman)
;; (mac-key-mode)
(setq mac-option-modifier 'meta)
;;(setq mac-command-key-is-meta nil)

(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;
;; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load some handy extensions

;;;;;;;;;;;;;;;;
;; ack
(require 'ack)

;;;;;;;;;;;;;;;;
;; flyspell

;; turn on flyspell mode
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq-default flyspell-mode t)


;;;;;;;;;;;;;;;;
;; paraedit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)


;;;;;;;;;;;;;;;;
;; magit (git support)
(require 'magit)



;;;;;;;;;;;;;;;;
;; auto-load for flyspell mode
(dolist (hook '(markdown-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;;;;;;;;;;;;;;;;
;; light symbol
(autoload 'light-symbol "light-symbol" "Float-over highlighting for symbols." t)

;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;
;; load up msf-abbrevs
(add-to-list 'load-path "~/work/emacs/msf-abbrev")
;; ensure abbrev mode is always on
(setq-default abbrev-mode t)

;; do not bug me about saving my abbreviations
;;(setq save-abbrevs nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; associations

;; add markdown mode automatically
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;
;; html mode for HTML fils

(setq auto-mode-alist (cons '("\\.html?$" . html-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;
;; Rake files are ruby too
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-em up

(require 'shell)
(require 'ruby-mode)

;;;;;;;;;;;;;;;;
;; msf-abbrev
(require 'msf-abbrev)
(setq msf-abbrev-verbose t) ;; optional
(setq msf-abbrev-root "~/work/emacs/msf-abbrev/abbrevs")
(global-set-key (kbd "C-c l") 'msf-abbrev-goto-root)
(global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
(msf-abbrev-load)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; little found functions

;; found world count function
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime

;; (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
;; (require 'slime)
;; (push (list 'clisp-2.35 (list "~/bin/lispbox-0.7/clisp-2.35/bin/clisp" "-ansi" "-K" "full" "-B" "/Applications/Lispbox/clisp-2.35/lib/clisp")) slime-lisp-implementations)
;; (slime-setup)
