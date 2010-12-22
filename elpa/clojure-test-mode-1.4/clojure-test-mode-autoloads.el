;;; clojure-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (clojure-test-mode) "clojure-test-mode" "clojure-test-mode.el"
;;;;;;  (19578 61631))
;;; Generated autoloads from clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-test-mode" "\
A minor mode for running Clojure tests.

\(fn &optional ARG)" t nil)

(defun clojure-test-maybe-enable nil "\
Enable clojure-test-mode if the current buffer contains Clojure tests.
Also will enable it if the file is in a test directory." (save-excursion (save-window-excursion (goto-char (point-min)) (when (search-forward "clojure.test" nil t) (clojure-test-mode t)))))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;;;### (autoloads nil nil ("clojure-test-mode-pkg.el") (19578 61631
;;;;;;  365471))

;;;***

(provide 'clojure-test-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clojure-test-mode-autoloads.el ends here
