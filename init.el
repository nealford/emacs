# 2011-01-15: moving infrastructure to dropbox


(setq load-path (cons "~/.emacs.d" load-path))

;; language modes  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scala ;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/scala")  
(require 'scala-mode-auto)

;; customizations  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set up alt key to work as META on Mac
(set-keyboard-coding-system 'mac-roman)
;; (mac-key-mode)
(setq mac-option-modifier 'meta)
;;(setq mac-command-key-is-meta nil)

(put 'downcase-region 'disabled nil)

;; always use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; color theme
(add-to-list 'load-path "~/.emacs.d/color-theme")
(require 'color-theme)
(color-theme-initialize)
;(color-theme-arjen)
;(color-theme-charcoal-black)
;(color-theme-billw)
;(color-theme-clarity)

;; handy extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; winring - for save/restore window configurations
;(require 'winring)
;(winring-initialize)

;; ack 
(require 'ack)    

;; docbook
(autoload 'docbook-xml-mode "docbook-xml-mode" "Major mode for Docbook" t)

;; flyspell
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq-default flyspell-mode t)
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
(require 'magit)

;; snippets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yasnippet
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

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
(color-theme-hober)
