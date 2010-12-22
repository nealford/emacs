(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 208 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(default-frame-alist (quote ((vertical-scroll-bars . right) (cursor-type . box) (background-toolbar-color . "#cccccccccccc") (bottom-toolbar-shadow-color . "#7a7a7a7a7a7a") (top-toolbar-shadow-color . "#f5f5f5f5f5f5") (color-theme-name . color-theme-arjen) (modeline . t) (fringe) (background-mode . dark) (menu-bar-lines . 1) (right-fringe . 11) (left-fringe . 3) (border-color . "black") (cursor-color . "yellow") (mouse-color . "sienna1") (background-color . "black") (foreground-color . "White") (font . "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1") (tool-bar-lines . 0))))
 '(longlines-wrap-follows-window-size t)
 '(mac-font-panel-mode nil)
 '(ns-command-modifier (quote alt))
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(nxml-slash-auto-complete-flag t)
 '(nxml-syntax-highlight-flag t)
 '(show-paren-mode nil)
 '(speedbar-use-images nil)
 '(tabbar-mode nil nil (tabbar))
 '(text-mode-hook (quote (smart-spacing-mode text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(visual-line-mode nil t))

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
;; Check custom-file compatibility
(when (and (boundp 'aquamacs-version-id)
           (< (floor (/ aquamacs-version-id 10))
	   (floor (/ aquamacs-customization-version-id 10))))
  (defadvice frame-notice-user-settings (before show-version-warning activate)
    (defvar aquamacs-backup-custom-file nil "Backup of `custom-file', if any.")
    (setq aquamacs-backup-custom-file "~/Library/Preferences/Aquamacs Emacs/customizations.1.9.el")
    (let ((msg "Aquamacs options were saved by a more recent program version.
Errors may occur.  Save Options to overwrite the customization file. The original, older customization file was backed up to ~/Library/Preferences/Aquamacs Emacs/customizations.1.9.el."))
      (if window-system
	  (x-popup-dialog t (list msg '("OK" . nil) 'no-cancel) "Warning")
	(message msg)))))
;; End compatibility check
