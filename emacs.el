
(put 'set-goal-column 'disabled nil)

(set-keyboard-coding-system 'mac-roman)

(setq mac-option-modifier 'meta)

(put 'downcase-region 'disabled nil)

(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-x C-p") 'other-window-backward)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "<f5>") 'lusty-file-explorer)
(global-set-key (kbd "<f6>") 'lusty-buffer-explorer)

(tabbar-mode 0)

(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)

(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)

;(color-theme-arjen)
;(color-theme-charcoal-black)
;(color-theme-billw)
(color-theme-clarity)
;(color-theme-calm-forest)
;(color-theme-lethe)

(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-directory "~/work/ppap/Notes,_Minutes,_Ideas,_Outlines/outlines")
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg")

(defun clean-code nil "Clean up code listings for XML embedation"
(interactive)
(query-replace "<" "lt;")
(query-replace ">" "gt;")
(query-replace "&" "&amp;"))

(defun other-window-backward (&optional n)
  "Select the previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(defun word-count nil "Count words in buffer" (interactive)
(shell-command-on-region (point-min) (point-max) "wc -w"))

(setenv "MITSCHEME_LIBRARY_PATH"  "/Applications/mit-scheme.app/Contents/Resources")

(add-to-list 'load-path "~/.emacs.d/scala")  
(require 'scala-mode-auto)

(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; some path munging to make Groovy happier
(defvar lib-dir "/Users/nford/bin/")
(setenv "GROOVY_HOME" (concat lib-dir "groovy-1.7.10"))
(setenv "PATH" (concat (getenv "PATH")
                       ":" (getenv "GROOVY_HOME") "/bin"))

(setenv "PATH" (concat (getenv "PATH") ":" lib-dir "markdown/markdown"))

(autoload 'docbook-xml-mode "docbook-xml-mode" "Major mode for
Docbook" t)

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq-default flyspell-mode t)
(dolist (hook '(markdown-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;; reset some keys to help recursive expansion
(define-key yas/keymap [tab] 'yas/expand)

(autoload 'word-count-mode "word-count"
          "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

(require 'emacsd-tile)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.html?$" . html-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
