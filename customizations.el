(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 190 t)
 '(default-frame-alist (quote ((tool-bar-lines . 0) (vertical-scroll-bars . right) (cursor-type . box) (background-toolbar-color . "#cccccccccccc") (bottom-toolbar-shadow-color . "#7a7a7a7a7a7a") (top-toolbar-shadow-color . "#f5f5f5f5f5f5") (color-theme-name . color-theme-arjen) (modeline . t) (fringe) (background-mode . dark) (menu-bar-lines . 1) (right-fringe . 11) (left-fringe . 3) (border-color . "black") (cursor-color . "yellow") (mouse-color . "sienna1") (background-color . "black") (foreground-color . "White") (font . "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1"))))
 '(longlines-wrap-follows-window-size t)
 '(mac-command-modifier (quote alt))
 '(mac-font-panel-mode nil)
 '(nxml-slash-auto-complete-flag t)
 '(nxml-syntax-highlight-flag t)
 '(show-paren-mode nil)
 '(speedbar-use-images nil)
 '(tabbar-mode nil nil (tabbar))
 '(text-mode-hook (quote (smart-spacing-mode text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(autoface-default ((t (:inherit default :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "apple-monaco"))))
 '(emacs-lisp-mode-default ((t (:inherit autoface-default))) t)
 '(markdown-mode-default ((t (:inherit text-mode-default :slant normal :weight normal :height 130 :family "bitstream vera sans mono"))) t)
 '(nxml-mode-default ((t (:inherit autoface-default :slant normal :weight normal :height 120 :family "bitstream vera sans mono"))) t))

;; for compatibility with older Aquamacs versions
 (defvar aquamacs-140-custom-file-upgraded t)
 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))

(setq aquamacs-default-styles (Arjen))