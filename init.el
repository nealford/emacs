;; add the locations of emacs-org/ other extenstions
(add-to-list 'load-path "~/.emacs.d/")

;; set-up org babel
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-babel-load-languages '((emacs-lisp . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(require 'org)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

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

;; load neatly organized org file!
(org-babel-load-file "~/.emacs.d/emacs-config.org")