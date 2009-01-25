(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 163 t)
 '(aquamacs-styles-mode t nil (color-theme))
 '(default-frame-alist (quote ((tool-bar-lines . 0) (vertical-scroll-bars . right) (cursor-type . box) (alpha 85 75) (background-toolbar-color . "#cccccccccccc") (bottom-toolbar-shadow-color . "#7a7a7a7a7a7a") (top-toolbar-shadow-color . "#f5f5f5f5f5f5") (color-theme-name . color-theme-arjen) (modeline . t) (fringe) (background-mode . dark) (menu-bar-lines . 1) (right-fringe . 11) (left-fringe . 3) (border-color . "black") (cursor-color . "yellow") (mouse-color . "sienna1") (background-color . "black") (foreground-color . "White") (font . "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1"))))
 '(nxml-slash-auto-complete-flag t)
 '(nxml-syntax-highlight-flag t)
 '(show-paren-mode nil)
 '(tabbar-mode nil nil (tabbar))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(transient-mark-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(markdown-mode-default ((t (:inherit text-mode-default :slant normal :weight normal :height 130 :family "monaco"))) t))

;; for compatibility with older Aquamacs versions
 (defvar aquamacs-140-custom-file-upgraded t)
 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))

(setq aquamacs-default-styles (Arjen))