(setq load-path (cons "~/.emacs.d" load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my stuff

(defun clean-code nil "Clean up code listings for XML embedation"
  (interactive)
  (query-replace "<" "lt;")
  (query-replace ">" "gt;")
  (query-replace "&" "&amp;"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix shell to not use zsh - broken for currently unexplored reasons
(setq shell-file-name "/bin/bash")

;; language modes  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scheme ;;;;;;;;;;;;;;;;
(setenv "MITSCHEME_LIBRARY_PATH"  "/Applications/mit-scheme.app/Contents/Resources")

;; scala ;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/scala")  
(require 'scala-mode-auto)

;; groovy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; some path munging to make Groovy happier
(defvar lib-dir "/Users/nford/bin/")
(setenv "GROOVY_HOME" (concat lib-dir "groovy-1.7.10"))

(setenv "PATH" (concat (getenv "PATH")
		       ":" (getenv "GROOVY_HOME") "/bin"))

;; and now path munging for Markdown
(setenv "PATH" (concat (getenv "PATH") ":" lib-dir "markdown/markdown"))

(defun other-window-backward (&optional n)
  "Select the previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(require 'emacsd-tile)

;; customizations  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'set-goal-column 'disabled nil)
;; set up alt key to work as META on Mac
(set-keyboard-coding-system 'mac-roman)
;; (mac-key-mode)
(setq mac-option-modifier 'meta)
;;(setq mac-command-key-is-meta nil)
(put 'downcase-region 'disabled nil)
;; add shortcut for wrap-region-with-tag
(global-set-key (kbd "C-c w") 'wrap-region-with-tag)

(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-x C-p") 'other-window-backward)

;; for org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-directory "~/work/ppap/Notes,_Minutes,_Ideas,_Outlines/outlines")
(setq org-mobile-inbox-for-pull "~/Dropbox/MobileOrg")


;; kill "tabbed editor windows" as a concept
(tabbar-mode 0)

;; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; replace shitty old buffer display with ibuff
(global-set-key (kbd "C-x C-b") 'ibuffer-list-buffers)

;; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
;(color-theme-arjen)
(color-theme-charcoal-black)
;(color-theme-billw)
;(color-theme-clarity)
;(color-theme-calm-forest)
;(color-theme-lethe)



;; make windows a little transparent
(setq transparency-level 90)
(set-frame-parameter nil 'alpha transparency-level)
(add-hook 'after-make-frame-functions (lambda (selected-frame) (set-frame-parameter selected-frame 'alpha transparency-level)))

;; handy extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ack 
(require 'ack)    

;; docbook
(autoload 'docbook-xml-mode "docbook-xml-mode" "Major mode for Docbook" t)

;; flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq-default flyspell-mode t)
;; killer flyspell-check-previous-highlighted-word keybinding
(global-set-key (kbd "C-c j") 'flyspell-check-previous-highlighted-word)

;; auto-load for flyspell mode
(dolist (hook '(markdown-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; light symbol
(autoload 'light-symbol "light-symbol" "Float-over highlighting for symbols." t)

;; magit (git support)
;;(require 'magit)

;; standard git support
(require 'git)
(require 'git-blame)

;; Sunrise Commander
(require 'sunrise-commander)
(require 'sunrise-x-buttons)

;; snippets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;; reset some keys to help recursive expansion
(define-key yas/keymap [tab] 'yas/expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snippets ;;

;; paraedit
;(autoload 'paredit-mode "paredit" "Minor mode for pseu  do-structurally editing Lisp code." t)

;; word count minor mode
;(setq load-path (cons (expand-file-name "~/.emacs.d") load-path))
(autoload 'word-count-mode "word-count"
          "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;; wrap-region
(add-to-list 'load-path "~/.emacs.d/wrap-region.el")
(require 'wrap-region)
(wrap-region-mode t)
(add-hook 'ruby-mode-hook
          '(lambda() (wrap-region-mode t)
))
(add-hook 'markdown-mode-hook
          '(lambda() (wrap-region-mode t)
))


;; associations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add markdown mode automatically
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; html mode for HTML fils
(setq auto-mode-alist (cons '("\\.html?$" . html-mode) auto-mode-alist))

;; Rake files are ruby too
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))


;; load-em up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'shell)
(require 'ruby-mode)

;; small (found) functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; found world count function
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

;;(defun fix-code-for-xml nil 
;;  "Escape troublesome characters in code listing in XML document" 
;;  (interactive)
;;  (while (search-forward

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(longlines-wrap-follows-window-size t)
 '(mac-command-modifier (quote alt))
 '(mac-font-panel-mode nil)
 '(speedbar-use-images nil)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
;; clojure-mode
(add-to-list 'load-path "~/opt/clojure-mode")
(require 'clojure-mode)
;; swank-clojure
;(add-to-list 'load-path "~/opt/swank-clojure")
;(require 'swank-clojure-autoload)
;(swank-clojure-config
; (setq swank-clojure-jar-path "~/.clojure/clojure.jar")
;  (setq swank-clojure-extra-classpaths
;         (list "~/.clojure/clojure-contrib.jar")))
;; slime
;(eval-after-load "slime"
;  '(progn (slime-setup '(slime-repl))))

;(add-to-list 'load-path "~/opt/slime")
;(require 'slime)
;(slime-setup)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;; set the theme; it got moved here to see if something else was clobbering it


