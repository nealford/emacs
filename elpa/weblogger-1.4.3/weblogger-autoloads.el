;;; weblogger-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (weblogger-start-entry weblogger-setup-weblog weblogger-select-configuration)
;;;;;;  "weblogger" "weblogger.el" (19347 65092))
;;; Generated autoloads from weblogger.el

(autoload (quote weblogger-select-configuration) "weblogger" "\
Select a previously saved configuration.

\(fn &optional CONFIG)" t nil)

(autoload (quote weblogger-setup-weblog) "weblogger" "\
Create a profile for a weblog.

\(fn)" t nil)

(autoload (quote weblogger-start-entry) "weblogger" "\
Start creating a weblog entry in the *weblogger-entry* buffer.
With a prefix, it will check the available weblogs on the server
and prompt for the weblog to post to if multiple ones are
available.

\(fn &optional PROMPT)" t nil)

;;;***

;;;### (autoloads nil nil ("weblogger-pkg.el") (19347 65092 198585))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; weblogger-autoloads.el ends here
