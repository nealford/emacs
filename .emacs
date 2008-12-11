;; setup load path
(setq load-path (cons "~/work/emacs" load-path))

;; turn on flyspell mode
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
;(setq-default flyspell-mode t)

;; turn on ido.el
(require 'ido)
(ido-mode t)

;; associations
;; add markdown mode automatically
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.html?$" . html-mode) auto-mode-alist))

;; load up msf-abbrevs
(add-to-list 'load-path "~/work/emacs/msf-abbrev")
;; ensure abbrev mode is always on
(setq-default abbrev-mode t)

;; do not bug me about saving my abbreviations
;;(setq save-abbrevs nil)

;; load up modes I use
(require 'shell)
(require 'ruby-mode)


(require 'msf-abbrev)
(setq msf-abbrev-verbose t) ;; optional
(setq msf-abbrev-root "~/work/emacs/msf-abbrev/abbrevs")
(global-set-key (kbd "C-c l") 'msf-abbrev-goto-root)
(global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
(msf-abbrev-load)

;; found world count function
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))


;; (add-hook 'slime-connected-hook 'slime-ensure-typeout-frame)
;; (require 'slime)
;; (push (list 'clisp-2.35 (list "~/bin/lispbox-0.7/clisp-2.35/bin/clisp" "-ansi" "-K" "full" "-B" "/Applications/Lispbox/clisp-2.35/lib/clisp")) slime-lisp-implementations)
;; (slime-setup)


(put 'downcase-region 'disabled nil)
