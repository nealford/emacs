;;; nterm.el --- New TERMinal emulator

;; Copyright (C) 2009 Ivan Kanis

;; Author: Ivan Kanis <look-for-me@your-favorite-search.engine>
;; Maintainer: Ivan Kanis <look-for-me@your-favorite-search.engine>
;; Created: 1 Oct 2009
;; Version: 0.1
;; Keywords: terminal shell

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Copyright is a the bottome of this file

;; Nterm is meant to be a full vt100 compatible terminal emulator. It
;; has the following features:

;;  - G0 G1 switching with shift in and shift out
;;  - special graphics characters (used for line drawing)
;;  - US and UK character set.
;;  - blinking, bright, underline and reverse rendition
;;  - scroll up and down including within top and bottom margin
;;  - switch terminal background color
;;  - switch between 80 and 132 columns screen
;;  - tabulation set and reset
;;  - all VT100 escape sequences are handled

;; Things that remains to do:
;;  - Double width character
;;  - Double height character
;;  - ANSI color
;;  - VT52 compatibility mode
;;  - Copy paste mechanism
;;  - xterm emulator

;; I think nterm is easier to maintain than term. One look at term's
;; term-emulate-terminal function should convince anyone that term
;; cannot be maintained anymore. Compare with nterm equivalent
;; function nterm-emulate it is only 25 lines long.

;; It has a recording mode (f11) so that you can record and replay
;; traces. It has a terminal memory so that area of the terminal can
;; be redrawn for blinking and changing screen background. There is a
;; memory dump mode (f10) that allows the programmer to examine the
;; memory.

;; The recorder takes a trace of characters received by the
;; terminal. Someone can easily reproduce a bug by replaying the trace
;; both on xterm and nterm.

;; It's not complete yet but it passes the first three tests of
;; vttest. I will get back to it when I have time.

;; The latest version is at http://kanis.fr/hg/lisp/ivan/nterm.el

;;; THANKS:

;;; BUGS:

;;; INSTALLATION:

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; Placate compiler
  (defvar nterm-dispatch)
  (defvar nterm-process)
  (defvar nterm-argument)
  (defvar nterm-vt100-primary-dispatch)
  (defvar nterm-vt100-escape-dispatch)
  (defvar nterm-vt100-bracket-dispatch)
  (defvar nterm-vt100-hash-dispatch)
  (defvar nterm-vt100-open-parenthesis-dispatch)
  (defvar nterm-vt52-escape-y-dispatch-line)
  (defvar nterm-vt52-escape-y-dispatch-col))

(defvar nterm-mode-hook nil
  "Hook run when entering nterm mode.")

(defvar nterm-shell "/bin/bash"
  "Name of the shell to run.")

(defvar nterm-buffer-name "*nterm*"
  "Name of the terminal buffer.")

(defvar nterm-height 24
  "Terminal height in lines.")

(defvar nterm-width 80
  "Terminal width in characters.")

(defvar nterm-double-width-alist
  (let ((char ?!)
        (unicode ?\uff01)
        (list '(( ?\s  . ?\u3000)))) ;; double width space
    (while (<= char ?~)
      (if (not (eq char ?s))
          (setq list (cons (cons char unicode) list)))
      (incf char)
      (incf unicode))
    list)
  "Map single width character to unicode double width equivalent.")

;;; Debugging
(defvar nterm-debug-emulator 1)
(defvar nterm-debug-vt100 2)
(defvar nterm-debug-cursor 4)
(defvar nterm-debug-ansi 8)
(defvar nterm-debug-vt52 16)
;; TBD change debug to a bool vector
;; (setq nterm-debug nterm-debug-emulator)
;; (setq nterm-debug nterm-debug-vt100)
;; (setq nterm-debug nterm-debug-cursor)
;; cursor + emulator
;; (setq nterm-debug 5)
;; emulator + vt100
;; (setq nterm-debug 3)
;; turn off debugging
;; (setq nterm-debug 0)

(defvar nterm-debug 0
  "Debugging variable.
You can set to the following value:
'nterm-debug-sgr debug Select graphic rendition
'nterm-debug-emulator debug emulator")

(defvar nterm-record-enable nil
  "Enable recording")

(defvar nterm-ansi-color
  ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "white"])

(defvar nterm-memory nil
  "The memory of the terminal, not for customization")

(defvar nterm-key-preserve '(?)
  "List of keys to preserve")

;; From http://www.inwap.com/pdp10/ansicode.txt
(defvar nterm-dispatch nil
  "Current dispatch table")

(defvar nterm-blink-time 1
  "Time in second for blinking.
Set to nil if you want to disable blinking")
(defmacro nterm-defdispatch (dispatch-list)
  "Create a dispatch table from DISPATCH-LIST."
  `(let* ((dispatch-name (symbol-name (nth 0 ,dispatch-list)))
          (dispatch-length (nth 1 ,dispatch-list))
          (dispatch-default (nth 2 ,dispatch-list))
          (dispatch-handlers (nthcdr 3 ,dispatch-list))
          (dispatch-result (make-vector dispatch-length dispatch-default)))
     (while dispatch-handlers
       (aset dispatch-result (car dispatch-handlers)
             (cadr dispatch-handlers))
       (setq dispatch-handlers (cddr dispatch-handlers)))
     (set (intern dispatch-name) dispatch-result)))

;;;###autoload
(defun nterm ()
  (interactive)
  (if (get-buffer nterm-buffer-name)
      (switch-to-buffer nterm-buffer-name)
    (nterm-mode)))

(defun nterm-emulate (process output)
  "Dispatch characters from process"
  (if (= (logand nterm-debug nterm-debug-emulator) nterm-debug-emulator)
      (message output))
  (if nterm-record-enable
      (nterm-record-insert output))
  (let ((emulate-index 0)
        (emulate-length (length output))
        (emulate-dispatch nil)
        (emulate-char ?0)
        (emulate-buffer (current-buffer)))
    (set-buffer nterm-buffer-name)
    (while (< emulate-index emulate-length)
      (setq emulate-char (aref output emulate-index))
      (if (< emulate-char (length (eval nterm-dispatch)))
          (progn
            (setq emulate-dispatch (aref (eval nterm-dispatch) emulate-char))
            (if emulate-dispatch
                (progn
                  (if (= (logand nterm-debug nterm-debug-emulator)
                         nterm-debug-emulator)
                      (message "received 0x%x %c dispatch %S"
                               emulate-char emulate-char emulate-dispatch))
                  (funcall emulate-dispatch emulate-char))
              (if (= (logand nterm-debug nterm-debug-emulator)
                     nterm-debug-emulator)
                  (message "received 0x%x not handled" emulate-char)))))
      (incf emulate-index))
    (set-buffer emulate-buffer)))

(defun nterm-mode ()
  "Major mode for emulating a terminal.
Entry to this mode runs the hooks on `nterm-mode-hook'."
  (interactive)
  (get-buffer-create nterm-buffer-name)
  (pop-to-buffer nterm-buffer-name)
  (kill-all-local-variables)
  (set (make-local-variable 'nterm-process)
       (get-buffer-process (current-buffer)))
  (set (make-local-variable 'nterm-argument) "")
  (setq mode-name "nterm")
  (setq major-mode 'nterm-mode)
  (setq truncate-lines t)
  (buffer-disable-undo nil)
  (nterm-blank-screen)
  (nterm-vt52-init)
  (nterm-vt100-init)
  (nterm-vt100-switch)
  (let* ((process-environment
          (nconc
           (list
            (format "TERM=vt100")) process-environment))
         (process-connection-type t)
         (inhibit-eol-conversion t)
         (coding-system-for-read 'binary)
         (process
          (start-process
           nterm-shell nterm-buffer-name
           nterm-shell "-c"
           (format "stty -nl echo rows %d columns %d sane ; exec %s"
                   nterm-height nterm-width nterm-shell))))
    (set-process-filter process 'nterm-emulate))
  (run-hooks 'nterm-mode-hook))

(defun nterm-dcdwl-char (char)
  "Return doublewidth of CHAR.
If it can't be found return a double width space"
  (let ((dcdwl-char (cdr (assq char nterm-double-width-alist))))
    (or dcdwl-char ?\u3000)))

;;; Cursor functions (all coordinate are 0 based)
(defun nterm-cursor-position-get ()
  "Return CONS of cursor-position (line . col)."
  (let ((cur-pos-line (nterm-cursor-line-get))
        (cur-pos-col (nterm-cursor-col-get)))
    (if (= (logand nterm-debug nterm-debug-cursor) nterm-debug-cursor)
        (message "nterm-cursor-position-get line=%d col=%d"
                 cur-pos-line cur-pos-col))
    (cons cur-pos-line cur-pos-col)))

(defun nterm-cursor-line-get ()
  "Returns the line number where the cursor is."
  (let ((get-line (- (line-number-at-pos) 1)))
    (if (> get-line nterm-height)
        (error "line out of range line=%d" get-line))
    (if (= (logand nterm-debug nterm-debug-cursor) nterm-debug-cursor)
        (message "nterm-cursor-line-get col=%d" get-line))
    get-line))

(defun nterm-cursor-col-get ()
  "Returns the column number where the cursor is."
  (let ((get-col (current-column)))
    (if (> get-col nterm-width)
        (error "col out of range col=%d" get-col))
    (if (= (logand nterm-debug nterm-debug-cursor) nterm-debug-cursor)
        (message "nterm-cursor-col-get col=%d" get-col))
    get-col))

(defun nterm-cursor-position-set (cursor-set)
  "Set cursor to cons CURSOR-SET (line . col)."
  (if (= (logand nterm-debug nterm-debug-cursor) nterm-debug-cursor)
      (message "nterm-cursor-position-set line=%d col=%d"
               (car cursor-set) (cdr cursor-set)))
  (nterm-cursor-col-set (cdr cursor-set))
  (nterm-cursor-line-set (car cursor-set)))

(defun nterm-cursor-line-set (cursor-line)
  "Move cursor to line CURSOR-LINE."
  (if (>= cursor-line nterm-height)
      (error "line out of range line=%d" cursor-line))
  (if (= (logand nterm-debug nterm-debug-cursor) nterm-debug-cursor)
      (message "nterm-cursor-line-set line=%d" cursor-line))
  (let ((set-line (- cursor-line (nterm-cursor-line-get))))
    (if (not (= set-line 0))
        (line-move set-line))))

(defun nterm-cursor-col-set (cursor-col)
  "Move cursor to column CURSOR-COL."
  (if (> cursor-col nterm-width)
      (error "col out of range col=%d" cursor-col))
  (if (= (logand nterm-debug nterm-debug-cursor) nterm-debug-cursor)
      (message "nterm-cursor-col-set col=%d" cursor-col))
  (goto-char (+ (line-beginning-position) cursor-col)))

;;; Scroll
(defun nterm-scroll-up (top bottom blank-line-function)
  "Scroll screen up from TOP to BOTTOM.
Use BLANK-LINE-FUNCTION to insert a blank line."
  (let ((up-pos (nterm-cursor-position-get)))
    (setcdr (nthcdr bottom nterm-memory)
            (cons (nterm-mem-line ?\s)
                  (nthcdr (+ bottom 1) nterm-memory)))
    (if (= top 0)
        ;; TBD optimise to avoid the copy
        (setq nterm-memory (cdr nterm-memory))
      (setcdr (nthcdr (- top 1) nterm-memory)
              (nthcdr (+ top 1) nterm-memory)))
    (nterm-cursor-position-set (cons top 0))
    (nterm-kill-line)
    (if (= bottom (- nterm-height 1))
        (progn
          ;; Handle last line, it doesn't end with a \n
          (goto-char (point-max))
          (insert "\n")
          (nterm-vt100-blank-line ?\s (- nterm-height 1)))
      (funcall blank-line-function ?\s bottom)
      (insert "\n"))
    (nterm-cursor-position-set up-pos)))

(defun nterm-scroll-down (top bottom blank-line-function)
  "Scroll screen down from TOP to BOTTOM.
Use BLANK-LINE-FUNCTION to insert a blank line."
  (let ((down-pos (nterm-cursor-position-get)))
    (if (= top 0)
        ;; TBD optimise to avoid the copy
        (setq nterm-memory (cons (nterm-mem-line ?\s) nterm-memory))
      (setcdr (nthcdr (- top 1) nterm-memory)
              (cons (nterm-mem-line ?\s) (nthcdr top nterm-memory))))
    (setcdr (nthcdr bottom nterm-memory)
            (nthcdr (+ bottom 2) nterm-memory))
    (nterm-cursor-position-set (cons bottom 0))
    (nterm-kill-line)
    (funcall blank-line-function ?\s top)
    (insert "\n")
    (nterm-cursor-position-set down-pos)))

;;; Miscelleneous
(defun nterm-keymap (key-function key-list keypad-list)
  "Returns keymap created with given arguments.
KEY-FUNCTION that will be called when a key is pressed.
KEY-LIST list of keys in kbd format that need to be handled.
KEY-PAD list of keypad keys in application and numeric mode."
  (let ((key-keymap (make-keymap))
        (key-index 0))
    (while (< key-index 128)
      (if (not (memq key-index nterm-key-preserve))
          (define-key key-keymap (vector key-index) key-function))
      (incf key-index))
    (while key-list
      (define-key key-keymap
        (read-kbd-macro (car key-list)) key-function)
      (setq key-list (cdr key-list)))
    (define-key key-keymap (kbd "<f10>") 'nterm-mem)
    (define-key key-keymap (kbd "<f11>") 'nterm-record)
    ;; keypad handling
    (while keypad-list
      (define-key key-keymap
        (read-kbd-macro
         (concat "<" (nth 0 (car keypad-list)) ">"))
        key-function)
      (let ((key-numlock-off (nth 1 (car keypad-list))))
        (if key-numlock-off
            (define-key key-keymap
              (read-kbd-macro
               (concat "<" key-numlock-off ">"))
              key-function)))
      (setq keypad-list (cdr keypad-list)))
    key-keymap))

(defun nterm-kill-line ()
  "Kill a line, don't push line in kill ring."
  (let ((kill-end (line-end-position)))
    (if (not (= kill-end (point-max)))
        (incf kill-end))
    (delete-region (point) kill-end )))

(defun nterm-blank-screen (&optional char)
  "Blank screen and memory with CHAR.
If char is not specified fill with space."
  (or char
      (setq char ?\s))
  (delete-region (point-min) (point-max))
  ;; blank screen
  (setq nterm-memory (make-list nterm-height nil))
  (let ((reset-index 0))
    (while (< reset-index nterm-height)
      (setcar (nthcdr reset-index nterm-memory) (nterm-mem-line char))
      (insert (make-string nterm-width char))
      (if (not (= reset-index (- nterm-height 1)))
          (insert "\n"))
      (incf reset-index)))
  (goto-char (point-min)))

;;; Key handling
(defun nterm-send-string (string)
  "Send STRING to host."
  (process-send-string nterm-process string))

(defun nterm-argument-collect (char)
  "Collect escape string."
  (setq nterm-argument (concat nterm-argument (char-to-string char))))

(defun nterm-argument-to-list (min-arg &optional default)
  "Return list of at least MIN-ARG
An element is set to DEFAULT if the argument was not specied.
It returns ARG-NUMBER of DEFAULT if arguments aren't enough"
  (or default (setq default 0))
  (let* ((bracket-index 0)
         (bracket-list (split-string nterm-argument ";"))
         (bracket-length (length bracket-list))
         (result-list nil))
    (setq nterm-argument "")
    (if (< bracket-length min-arg)
        (setq result-list(make-list min-arg default))
      (while (< bracket-index bracket-length)
        (setq result-list
              (append
               result-list
               (list (nterm-argument-default
                      (nth bracket-index bracket-list) default))))
        (incf bracket-index)))
    result-list))

(defun nterm-argument-default (number default)
  "Return number from string NUMBER.
Return DEFAULT is NUMER is 0"
  (let ((collect-number (string-to-number number)))
    (if (eq collect-number 0) default collect-number)))

;;; ANSI
(defvar nterm-ansi-mode (make-bool-vector 21 nil))

(defvar nterm-ansi-mode-function
  [
   nil  ; 0 unused
   nil  ; GATM 1 Guarded Area Transmit Mode, send all (VT132)
   nil  ; KAM 2 Keyboard Action Mode, disable keyboard input
   nil  ; CRM 3 Control Representation Mode, show all control chars
   nil  ; IRM 4 Insertion/Replacement Mode, set insert mode (VT102)
   nil  ; SRTM 5 Status Report Transfer Mode, report after DCS
   nil  ; ERM 6 ERasure Mode, erase protected and unprotected
   nil  ; VEM 7 Vertical Editing Mode, IL/DL affect previous lines
   nil  ; 8 reserved
   nil  ; 9 reserved
   nil  ; HEM 10 Horizontal Editing mode, ICH/DCH/IRM go backwards
   nil  ; PUM 11 Positioning Unit Mode, use decipoints for HVP/etc
   nil  ; SRM 12 Send Receive Mode, transmit without local echo
   nil  ; FEAM 13 Format Effector Action Mode, FE's are stored
   nil  ; FETM 14 Format Effector Transfer Mode, send only if stored
   nil  ; MATM 15 Multiple Area Transfer Mode, send all areas
   nil  ; TTM 16 Transmit Termination Mode, send scrolling region
   nil  ; SATM 17 Send Area Transmit Mode, send entire buffer
   nil  ; TSM 18 Tabulation Stop Mode, lines are independent
   nil  ; EBM 19 Editing Boundry Mode, all of memory affected
   nil] ; LNM 20 Linefeed Newline Mode, LF interpreted as CR LF
  "Ansi vector of function to call after changing mode.")

(defun nterm-ansi-mode-lnm ()
  (aref nterm-ansi-mode 1))

(defun nterm-ansi-rm (char)
  "RM -- Reset Mode - ansi"
  (if (= (logand nterm-debug nterm-debug-ansi) nterm-debug-ansi)
      (message "RM"))
  (if (nterm-vt100-set-mode
       nterm-ansi-mode nterm-ansi-mode-function nil)
      (nterm-vt100-escape-end char)))

(defun nterm-ansi-sm (char)
  "SM -- Set Mode - ansi"
  (if (= (logand nterm-debug nterm-debug-ansi) nterm-debug-ansi)
      (message "SM"))
  (nterm-vt100-set-mode
   nterm-ansi-mode nterm-ansi-mode-function t)
  (nterm-vt100-escape-end char))

(defun nterm-ansi-sgr (sgr-number))
;; "TBD"
;; (cond ((eq sgr-number 2) (nterm-color-faint))
;;       ((eq sgr-number 3) (nterm-face-modify :slant 'italic))
;;       ((eq sgr-number 6) (nterm-blink-fast))
;;       ((eq sgr-number 8) (nterm-concealed))
;;       ((eq sgr-number 9) (nterm-face-modify :strike_through t))
;;       ((< sgr-number 21) (nterm-alternative-font (- sgr-number 10)))
;;       ;; doubly underlined, but emacs can't do it
;;       ((eq sgr-number 21) (nterm-face-modify :underline t))
;;       ((eq sgr-number 22) (nterm-color-normal))
;;       ;; not fraktur according to ECMA-48, not implemented
;;       ((eq sgr-number 23) (nterm-face-modify :slant 'normal))
;;       ((eq sgr-number 24) (nterm-face-modify :underline nil))
;;       ((eq sgr-number 25) (nterm-steady))
;;       ;; 26 reserved for proportinal spacing
;;       ((eq sgr-number 27) (nterm-positive-image))
;;       ((eq sgr-number 28) (nterm-revealed))
;;       ((eq sgr-number 29) (nterm-face-modify :strike_through nil))
;;       ((< sgr-number 38) (nterm-foreground (- sgr-number 30)))
;;       ;; 38 reserved for future standardization
;;       ;; 39 default foreground : white
;;       ((eq sgr-number 39) (nterm-foreground 7))
;;       ((< sgr-number 48) (nterm-background (- sgr-number 40)))
;;       ;; 48 reserved for future standardization
;;       ;; 49 default background : black
;;       ((eq sgr-number 49) (nterm-background 0))
;;       ;; ECMA-48 50 to 65 can't be rendered with Emacs so don't bother
;;       (t nil)))

(defun nterm-face-modify (face key value)
  "Modify FACE at KEY with VALUE.
It returns the new face list."
  (let ((list face)
        (result nil)
        (b-key-found nil)
        elt)
    (while list
      (if b-key-found
          (progn
            (setq b-key-found nil)
            (setq elt (list value)))
        (setq elt (list (car list)))
        (if (eq key (car list))
            (setq b-key-found t)))
      (setq result (append result elt))
      (setq list (cdr list)))
    result))

;;; Macro
(defmacro nterm-face-bright (face)
  "Set bright propertiy on face."
  `(setq
    ,face
    (nterm-face-modify
     ,face
     :foreground
     (nth 1 (nth (nterm-vt100-mode-decscnm-1) nterm-vt100-color)))))

(defmacro nterm-face-underline (face)
  "Set underline properties on FACE."
  `(setq
    ,face
    (nterm-face-modify ,face :underline t)))

(defmacro nterm-face-reverse (face bright)
  "Set reverse property on FACE and BRIGHT."
  `(let ((reverse-bright (if ,bright 1 0)))
     (setq
      ,face
      (nterm-face-modify
       ,face
       :foreground
       (nth reverse-bright
            (nth (nterm-vt100-mode-decscnm-0) nterm-vt100-color)))
      ,face
      (nterm-face-modify
       ,face
       :background
       (nth reverse-bright
            (nth (nterm-vt100-mode-decscnm-1) nterm-vt100-color))))))

(defmacro nterm-state-copy (from to copy)
  "Copy FROM parameter of vt100 state TO paramter
If COPY is t copy parameter"
  `(setcdr
    (assq (quote ,to) nterm-vt100-state)
    (if ,copy
        (copy-sequence (cdr (assq (quote ,from) nterm-vt100-state)))
      (cdr (assq (quote ,from) nterm-vt100-state)))))

(defun nterm-color-faint ())
(defun nterm-color-normal ())
(defun nterm-steady())
(defun nterm-negative-image ())
(defun nterm-positive-image ())
(defun nterm-concealed ())
(defun nterm-revealed())
(defun nterm-alternative-font (index))
(defun nterm-foreground(index)
  "TBD ansi color")

(defun nterm-background(index))

;;; vt100 emulator
;; I have used the vt100 User Guide at
;; http://vt100.net/docs/vt100-ug/
(defvar nterm-vt100-state nil
  "Alist of vt100 attributes")

(defvar nterm-vt100-mode nil
  "Bool vector holding vt100 modes.")

(defvar nterm-vt100-mode-function
  [ nil                 ; 0 Error (ignored)
    nterm-vt100-decckm  ; 1 DECCKM Cursor key
    nterm-vt100-decanm  ; 2 DECANM ANSI/VT52
    nterm-vt100-deccolm ; 3 DECCOLM Column
    nil                 ; 4 DECSCLM Scrolling
    nterm-vt100-decscnm ; 5 DECSCNM Screen
    nterm-vt100-decom   ; 6 DECOM Origin
    nterm-vt100-decawm  ; 7 DECAWM Auto wrap
    nil                 ; 8 DECARM Auto repeating
    nil                 ; 9 DECINLM Interlace
    nil                 ; 10 DECKPAM Keypad Application
    ]
  "List of meanings of vt100 modes.")


(defvar nterm-vt100-c0
  [ ?\x00 ; NUL
    nil
    nil
    ?\x03 ; ETX
    ?\x04 ; EOT
    ?\x05 ; ENQ
    nil
    ?\x07 ; BEL
    ?\x08 ; BS
    ?\x09 ; HT
    ?\x0a ; LF
    ?\x0b ; VT
    ?\x0c ; FF
    ?\x0d ; CR
    ?\x0e ; SO
    ?\x0f ; SI
    nil
    ?\x11 ; DC1 (XON)
    nil
    ?\x13 ; DC2 (XOFF)
    nil
    nil
    nil
    nil
    ?\x18 ; CAN
    nil
    ?\x1a ; SUB
    ?\x1b ; ESC
    nil
    nil
    nil
    nil ]
  "vt100 C0 character set, nil means character is not used")

(defvar nterm-vt100-charset-special
  (vconcat
   nterm-vt100-c0
   (let ((start ?\s)
         (end ??)
         (index 0)
         (vec (make-vector 63 nil)))
     (while (< index end)
       (aset vec index start)
       (incf start)
       (incf index))
     vec)
   [ ?\s ; Blank
     ?♦ ; Diamond
     ?▒ ; Checkerboard (error indication)
     ?␉ ; Horizontal tab
     ?␌ ; Form feed
     ?␍ ; Carriage return
     ?␊ ; Linefeed
     ?° ; Degree symbol
     ?± ; Plus/minus
     ?␤ ; New line
     ?␋ ; Vertical tab
     ?┘ ; Lower-right corner
     ?┐; Upper-right corner
     ?┌ ; Upper-left corner
     ?└ ; Lower-left corner
     ?┼ ; Crossing lines
     ?⎺ ; Horizontal line scan 1
     ?⎻ ; Horizontal line scan 3
     ?─ ; Horizontal line scan 5
     ?⎼ ; Horizontal line scan 7
     ?⎽; Horizontal line scan 9
     ?├ ; Left T
     ?┤ ; Right T
     ?┴ ; Bottom T
     ?┬ ; Top T
     ?│ ; Vertical bar
     ?≤ ; Less than or equal to
     ?≥ ; Greater than or equal to
     ?π ; Pi
     ?≠ ; Not equal to
     ?£ ; UK pound sign
     ?· ; Centered dot
     ?\s ; delete
     ])
  " Special Characters and Line Drawing
http://www.vt100.net/docs/vt102-ug/table5-15.html
special characters starts at 95 and end at 126")

(defvar nterm-vt100-charset-us
  (vconcat nterm-vt100-c0
           (let ((g0-index 0)
                 (g0-list (make-vector 96 nil)))
             (while (< g0-index 95)
               (aset g0-list g0-index (+ ?\s g0-index))
               (incf g0-index))
             ;; map delete to space
             (aset g0-list 95 ?\s)
             g0-list))
  "United State character set")

(defvar nterm-vt100-charset-uk
  (let ((uk-list (copy-sequence nterm-vt100-charset-us)))
    (aset uk-list ?# ?£)
    uk-list)
  "United Kingdom character set")

(defvar nterm-vt100-color
  ;;  normal  bright
  '(("black"  "gray20")
    ("gray80" "white"))
  "Color of a vt100.
One of the most misunderstood term is bold. Bold is just a
brighter color on a VT terminal, it is not a bold font.")

(defvar nterm-vt100-keypad-table
  ;; numlock on, numlock off, numeric, application
  '(("kp-0"       "kp-insert"  "0"    "\eOp")
    ("kp-1"       "kp-end"     "1"    "\eOq")
    ("kp-2"       "kp-down"    "2"    "\eOr")
    ("kp-3"       "kp-next"    "3"    "\eOs")
    ("kp-4"       "kp-left"    "4"    "\eOt")
    ("kp-5"       "kp-begin"   "5"    "\eOu")
    ("kp-6"       "kp-right"   "6"    "\eOv")
    ("kp-7"       "kp-home"    "7"    "\eOw")
    ("kp-8"       "kp-up"      "8"    "\eOx")
    ("kp-9"       "kp-prior"   "9"    "\eOy")
    ("kp-subtract" nil         "-"    "\eOm")
    ("kp-multiply" nil         ","    "\eOl")
    ("kp-decimal"  "kp-delete" "."    "\eOn")
    ("kp-enter"    nil         "\r"   "\eOM")
    ("f1"          nil         "\eOP" "\eOP")
    ("f2"          nil         "\eOQ" "\eOQ")
    ("f3"          nil         "\eOR" "\eOR")
    ("f4"          nil         "\eOS" "\eOS"))
  "Table of auxiliary keypad codes of a vt100.
Comma is mapped to the * multiply key. A PC keyboard doesn't have
a comma in the keypad. Function keys are mapped to the PC
function keys. If you have a different keyboard map these keys to
your liking.")

(defvar nterm-vt100-mode-map nil
  "Nterm keymap for vt100, use `nterm-mode-hook' to customize.")

;;; Character attributes
(defvar nterm-vt100-char-bright 0)
(defvar nterm-vt100-char-underline 1)
(defvar nterm-vt100-char-blink 2)
(defvar nterm-vt100-char-reverse 3)

;;; Line attributes
(defvar nterm-vt100-line-decdwl 0
  "Line is double width")

(defvar nterm-vt100-line-blink 1
  "Line is blinking")

(nterm-defdispatch ; Primary dispatch of a VT100
 '(nterm-vt100-primary-dispatch
   128 nterm-vt100-char-self
   ? nterm-vt100-so
   ? nterm-vt100-si
   ?\a nterm-vt100-bel
   ?\b nterm-vt100-bs
   ?\e nterm-vt100-escape-start
   ?\n nterm-vt100-lf
   ?\r nterm-vt100-cr
   ?\t nterm-vt100-tab
   ?\v nterm-vt100-lf))

(nterm-defdispatch ; Escape (ESC) dispatch of a vt100
 '(nterm-vt100-escape-dispatch
   128 nterm-vt100-escape-end
   ?\e nil ; treat multiple esc as one (seen with aptitude)
   ?# nterm-vt100-hash-start
   ?\( nterm-vt100-parenthesis-open-start
   ?\) nterm-vt100-parenthesis-close-start
   ?7 nterm-vt100-decsc
   ?8 nterm-vt100-decrc
   ?= nterm-vt100-deckpam
   ?> nterm-vt100-deckpnm
   ?D nterm-vt100-ind
   ?E nterm-vt100-nel
   ?H nterm-vt100-hts
   ?M nterm-vt100-ri
   ?Z nterm-vt100-decid
   ?\[ nterm-vt100-bracket-start
   ?c nterm-vt100-ris))

(nterm-defdispatch ; Bracket dispatch ESC [
 '(nterm-vt100-bracket-dispatch
   128 nterm-vt100-escape-end
   ?\a nterm-vt100-bel
   ?\b nterm-vt100-bs
   ?\n nterm-vt100-lf
   ?\r nterm-vt100-cr
   ?\t nterm-vt100-tab
   ?\v nterm-vt100-lf
   ?0 nterm-argument-collect
   ?1 nterm-argument-collect
   ?2 nterm-argument-collect
   ?3 nterm-argument-collect
   ?4 nterm-argument-collect
   ?5 nterm-argument-collect
   ?6 nterm-argument-collect
   ?7 nterm-argument-collect
   ?8 nterm-argument-collect
   ?9 nterm-argument-collect
   ?\; nterm-argument-collect
   ?? nterm-vt100-question-start
   ?A nterm-vt100-cuu
   ?B nterm-vt100-cud
   ?C nterm-vt100-cuf
   ?D nterm-vt100-cub
   ?H nterm-vt100-cup
   ?J nterm-vt100-ed
   ?K nterm-vt100-el
   ?M nterm-vt100-ri
   ?c nterm-vt100-da
   ?f nterm-vt100-hvp
   ?g nterm-vt100-tbc
   ?h nterm-ansi-sm
   ?l nterm-ansi-rm
   ?m nterm-vt100-sgr
   ?n nterm-vt100-dsr
   ?q nterm-vt100-decll
   ?r nterm-vt100-decstbm
   ?x nterm-vt100-decreqtparm
   ?y nterm-vt100-dectst))

(nterm-defdispatch ; Question dispatch ESC [ ?
 '(nterm-vt100-question-dispatch
   128 nterm-vt100-escape-end
   ?0 nterm-argument-collect
   ?1 nterm-argument-collect
   ?2 nterm-argument-collect
   ?3 nterm-argument-collect
   ?4 nterm-argument-collect
   ?5 nterm-argument-collect
   ?6 nterm-argument-collect
   ?7 nterm-argument-collect
   ?8 nterm-argument-collect
   ?9 nterm-argument-collect
   ?h nterm-vt100-sm
   ?l nterm-vt100-rm))


(nterm-defdispatch ; Hash dispatch ESC # of a vt100
 '(nterm-vt100-hash-dispatch
   128 nterm-vt100-escape-end
   ?3 nterm-vt100-decdhl-top
   ?4 nterm-vt100-decdhl-bottom
   ?5 nterm-vt100-decswl
   ?6 nterm-vt100-decdwl
   ?8 nterm-vt100-decaln))

(nterm-defdispatch ; Open parenthesis dispatch ESC ( of a vt100
 '(nterm-vt100-parenthesis-open-dispatch
   128 nterm-vt100-escape-end
   ?0 nterm-vt100-scs-g0
   ?1 nterm-vt100-scs-g0
   ?2 nterm-vt100-scs-g0
   ?A nterm-vt100-scs-g0
   ?B nterm-vt100-scs-g0))

(nterm-defdispatch ; Close parenthesis dispatch ESC ) of a vt100
 '(nterm-vt100-parenthesis-close-dispatch
   128 nterm-vt100-escape-end
   ?0 nterm-vt100-scs-g1
   ?1 nterm-vt100-scs-g1
   ?2 nterm-vt100-scs-g1
   ?A nterm-vt100-scs-g1
   ?B nterm-vt100-scs-g1))

(defun nterm-vt100-bel (char)
  "Bell."
  (ding))

(defun nterm-vt100-blank-line (char line &optional length replace)
  "Insert LENGTH of CHAR at LINE.
If LENGTH is nil use the terminal width If REPLACE is t
characters are overwritten. LINE is 0 based. The cursor is left
at the end of the line"
  (or length
      (progn
        (setq length nterm-width)
        (nterm-cursor-col-set 0)))
  (nterm-cursor-line-set line)
  (let* ((line-decdwl (aref (cdr (assq 'line-attr (nth line nterm-memory)))
                            nterm-vt100-line-decdwl))
         (line-length (if line-decdwl (/ length 2) length))
         (line-index 0))
    (while (< line-index line-length)
      (nterm-vt100-char-insert char replace)
      (incf line-index))))

(defun nterm-vt100-blink-screen ()
  "Blink timer, handle blinking on the screen."
  (let ((blink-index 0)
        (blink-exist nil)
        (blink-buffer (current-buffer))
        (blink (assq 'blink nterm-vt100-state)))
    (set-buffer nterm-buffer-name)
    (setcdr blink (not (cdr blink)))
    (while (< blink-index nterm-height)
      (if (aref (cdr (assq 'line-attr (nth blink-index nterm-memory)))
                nterm-vt100-line-blink)
          (progn
            (nterm-vt100-line-draw blink-index t)
            (setq blink-exist t)))
      (incf blink-index))
    (if (not blink-exist)
        (cancel-timer (cdr (assq 'blink-timer nterm-vt100-state))))
    (set-buffer blink-buffer)))

(defun nterm-vt100-bracket-start (char)
  "Escape bracket dispatch"
  (setq nterm-dispatch 'nterm-vt100-bracket-dispatch))

(defun nterm-vt100-bs (char)
  "Backspace, it doesn't erase in VT100"
  (let* ((cub-col (- (nterm-cursor-col-get) 1)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "Backspace"))
    (if (< cub-col 0)
        (setq cub-col 0))
    (nterm-cursor-col-set cub-col)
    ;; end wrapping
    (setcdr (assq 'wrap nterm-vt100-state) nil)))

(defun nterm-vt100-cpr ()
  "CPR -- Cursor Position Report -- vt100 to host.
TBD implement DECOM"
  (let* ((cpr-line (+ (nterm-cursor-line-get) 1))
         (cpr-col (+ (nterm-cursor-col-get) 1))
         (string (format "\e[%s;%s)" cpr-line cpr-col)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "CPR line=%d col=%d" cpr-line cpr-col))
    (nterm-send-string string)))

(defun nterm-vt100-cub (char)
  "CUB -- Cursor Backward -- host to vt100"
  (let* ((cub-number (car (nterm-argument-to-list 1 1)))
         (cub-col (- (nterm-cursor-col-get) cub-number)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "CUB %d" cub-number))
    (if (< cub-col 0)
        (setq cub-col 0))
    (nterm-cursor-col-set cub-col))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-cud (char)
  "CUD -- Cursor Down -- host to vt100"
  (let* ((cud-number (car (nterm-argument-to-list 1 1)))
         (cud-line (+ (nterm-cursor-line-get) cud-number))
         (cud-height (cdr (assq 'bottom-margin nterm-vt100-state))))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "CUD %d" cud-number))
    (if (> cud-line cud-height)
        (setq cud-line cud-height))
    (nterm-cursor-line-set cud-line))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-cuf (char)
  "CUF -- Cursor Forward -- host to vt100"
  (let* ((cuf-number (car (nterm-argument-to-list 1 1)))
         (cuf-col (+ (nterm-cursor-col-get) cuf-number))
         (cud-width (- nterm-width 1)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "CUF %d" cuf-number))
    (if (> cuf-col cud-width)
        (setq cuf-col cud-width))
    (nterm-cursor-col-set cuf-col))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-cup (char)
  "CUP -- Cursor Position -- host to vt100"
  (let* ((cup-list (nterm-argument-to-list 2 1))
         (cup-line (nth 0 cup-list))
         (cup-col (nth 1 cup-list)))
    ;; add top margin when DECOM is active
    (if (nterm-vt100-mode-decom)
        (setq cup-line (+ cup-line
                          (cdr (assq 'top-margin nterm-vt100-state)))))
    ;; check for  upper bound
    (and (> cup-line nterm-height) (setq cup-line 1))
    (and (> cup-col nterm-width) (setq cup-col 1))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "CUP line=%d col=%d" cup-line cup-col))
    (nterm-cursor-position-set (cons (- cup-line 1) (- cup-col 1))))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-cuu (char)
  "CUU -- Cursor Up -- host to vt100"
  (let* ((cuu-number (car (nterm-argument-to-list 1 1)))
         (cuu-line (- (nterm-cursor-line-get) cuu-number))
         (cuu-top (cdr (assq 'top-margin nterm-vt100-state))))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "CUU %d" cuu-number))
    (if (< cuu-line cuu-top)
        (setq cuu-line cuu-top))
    (nterm-cursor-line-set cuu-line))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-char-insert (char &optional replace)
  "Insert character, with face property"
  (let* ((insert-line (nterm-cursor-line-get))
         (insert-col (nterm-cursor-col-get))
         (insert-line-attribute
          (cdr (assq 'line-attr
                     (nth insert-line nterm-memory))))
         (insert-decdwl
          (aref insert-line-attribute nterm-vt100-line-decdwl))
         (insert-mem-char (cdr (assq 'char (nth insert-line nterm-memory))))
         (insert-mem-attribute
          (cdr (assq 'attr (nth insert-line nterm-memory))))
         (insert-char-table (cdr (assq 'char-table nterm-vt100-state)))
         (insert-char (if (< char (length insert-char-table))
                          (aref insert-char-table char)))
         (insert-attribute (cdr (assq 'attribute nterm-vt100-state))))
    (aset insert-mem-char insert-col insert-char)
    (aset insert-mem-attribute insert-col (copy-sequence insert-attribute))
    (if replace
        (delete-char 1))
    (insert
     (char-to-string
      ;; TBD need to output double width special characters
      (if insert-decdwl (nterm-dcdwl-char char) insert-char)))
    (put-text-property (- (point) 1) (point) 'face
                       (cdr (assq 'face nterm-vt100-state)))
    ;; Blink
    (if (aref insert-attribute nterm-vt100-char-blink)
        (progn
          (aset insert-line-attribute nterm-vt100-line-blink t)
          (if (and nterm-blink-time
                   (not (cdr (assq 'blink-timer nterm-vt100-state))))
              (setcdr (assq 'blink-timer nterm-vt100-state)
                      (run-at-time nil nterm-blink-time
                                   'nterm-vt100-blink-screen)))))))

(defun nterm-vt100-char-self (char)
  "Insert character from output.
Take care of wrapping."
  ;; wrapping
  (if (and (nterm-vt100-mode-decawm)
           (cdr (assq 'wrap nterm-vt100-state))
           (= (nterm-cursor-col-get) (- nterm-width 1)))
      (progn
        (nterm-vt100-lf char)
        (nterm-vt100-cr char)))
  (setcdr (assq 'wrap nterm-vt100-state) nil)
  (nterm-vt100-char-insert char t)
  ;; post insert logic
  (if (= (point) (line-end-position))
      (progn
        (nterm-cursor-col-set (- nterm-width 1))
        (setcdr (assq 'wrap nterm-vt100-state) t))))

(defun nterm-vt100-cr (char)
  "Do a carriage return"
  (interactive)
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "CR"))
  (move-beginning-of-line nil))

(defun nterm-vt100-da (char)
  "DA -- Device attribute TBD"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DA"))
  (nterm-send-string"\e[?1;2c")
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decaln (char)
  "DECALN -- Screen Alignment Display (DEC Private)"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECALN"))
  (nterm-vt100-reset ?E)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decanm (flag)
  "DECANM – ANSI/VT52 Mode (DEC Private)"
  (if flag
      (nterm-vt100-switch)
    (nterm-vt52-switch)))

(defun nterm-vt100-decawm (flag)
  "DECAWM – Auto Wrap Mode (DEC Private)"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECAWM %s" (if flag "set" "reset"))))

(defun nterm-vt100-decckm (flag)
  "DECCKM – Column Mode (DEC Private)"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECCKM %s" (if flag "set" "reset"))))

(defun nterm-vt100-deccolm (flag)
  "DECCOLM – Column Mode (DEC Private)"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECCOLM %s" (if flag "set" "reset")))
  (let ((deccolm-width (if flag 132 80)))
    (set-frame-width nil deccolm-width)
    (setq nterm-width deccolm-width)
    (nterm-vt100-reset ?\s)))

(defun nterm-vt100-decdhl-top (char)
  "DECDHL -- Double Height Line (DEC Private)
TBD This is the top part.
How on earth could we implement this for emacs?
Note that the font is double width"
  (message "DECHDL (top)")
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decdhl-bottom (char)
  "DECDHL -- Double Height Line (DEC Private)
TBD This is the bottom part"
  (message "DECHDL (bottom)")
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decdwl (char)
  "DECDWL -- Double-Width Line (DEC Private)"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECDWL"))
  (let ((decdwl-line (+ (nterm-cursor-line-get) 1)))
    (aset (cdr (assq 'line-attr (nth decdwl-line nterm-memory)))
          nterm-vt100-line-decdwl t)
    (nterm-vt100-blank-line ?\s decdwl-line nil t)
    (forward-line -1)
    (nterm-vt100-escape-end char)))

(defun nterm-vt100-decid (char)
  "DECID -- Identify Terminal (DEC Private)
TBD implement"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECID"))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-deckpam (char)
  "DECKPAM -- Keypad Application Mode (DEC Private)."
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECKPAM"))
  (aset nterm-vt100-mode 10 t)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-deckpnm (char)
  "DECKPNM -- Keypad Numeric Mode (DEC Private)."
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECKPNM"))
  (aset nterm-vt100-mode 10 nil)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decll (char)
  "DECLL -- Load LEDS (DEC Private)
TBD implement"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECKLL")
    (nterm-vt100-escape-end char)))

(defun nterm-vt100-decom (flag)
  "DECOM – Origin Mode (DEC Private)"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECOM %s" (if flag "set" "reset")))
  (nterm-vt100-home))

(defun nterm-vt100-decrc (char)
  "DECRC -- Restore Cursor (DEC Private) - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "RC"))
  (nterm-cursor-position-set (cdr (assq 'save-cursor nterm-vt100-state)))
  (nterm-state-copy save-charset charset nil)
  (nterm-state-copy save-face face nil)
  (nterm-state-copy save-char-table char-table nil)
  (nterm-state-copy save-attribute attribute nil)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decreptparm ()
  "DECREPTPARM -- Report Terminal Parameters - vt100 to host
TBD implement"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "REPTPARM")))

(defun nterm-vt100-decreqtparm (char)
  "DECREQTPARM -- Request Terminal Parameters - host to vt100
TBD implement"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECREQTPARM"))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decsc (char)
  "DECSC -- Save Cursor (DEC Private) - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECSC"))
  (setcdr (assq 'save-cursor nterm-vt100-state)
          (nterm-cursor-position-get))
  (nterm-state-copy charset save-charset t)
  (nterm-state-copy face save-face t)
  (nterm-state-copy char-table save-char-table nil)
  (nterm-state-copy attribute save-attribute t)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decscnm (flag)
  (setcdr (assq 'face nterm-vt100-state) (nterm-vt100-face-default))
  (let ((scnm-index 0))
    (while (< scnm-index nterm-height)
      (nterm-vt100-line-draw scnm-index t)
      (incf scnm-index))))

(defun nterm-vt100-decstbm (char)
  "DECSTBM -- Set Top and Bottom Margins (DEC Private) - host to vt100"
  (let* ((stbm-list (nterm-argument-to-list 2 0))
         (stbm-top (nth 0 stbm-list))
         (stbm-bottom (nth 1 stbm-list)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "DECSTBM top=%d bottom=%d" stbm-top stbm-bottom))
    (if (= stbm-bottom 0)
        (setq stbm-bottom (- nterm-height 1))
      (setq stbm-bottom (- stbm-bottom 1)))
    (if (not (= stbm-top 0))
        (decf stbm-top))
    (if (< stbm-top stbm-bottom)
        (progn
          (setcdr (assq 'top-margin nterm-vt100-state) stbm-top)
          (setcdr (assq 'bottom-margin nterm-vt100-state) stbm-bottom)
          (nterm-vt100-home))))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-decswl (char)
  "DECSWL -- Single-width Line (DEC Private) - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECSWL"))
  (let ((decdwl-line (+ (nterm-cursor-line-get) 1)))
    (aset (cdr (assq 'line-attr (nth decdwl-line nterm-memory)))
          nterm-vt100-line-decdwl nil)
    (nterm-vt100-blank-line ?\s decdwl-line nil t))
  (forward-line -1)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-dectst (char)
  "DECTST -- Invoke Confidence Test - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECTST"))
  (let ((list-dectst (nterm-argument-to-list 2 0)))
    (if (and (eq (nth 0 list-dectst) 2)
             (eq (nth 1 list-dectst) 0))
        (nterm-vt100-reset ?\s))))

(defun nterm-vt100-dsr (char)
  "DSR -- Device Status Report"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "DECDSR"))
  (let ((dsr-number (car (nterm-argument-to-list 1 0))))
    (cond ((eq dsr-number 0)
           (nterm-send-string "\e[0n"))
          ((eq dsr-number 6)
           (nterm-vt100-cpr)))))

(defun nterm-vt100-ed (char)
  "ED -- Erase In Display - host to vt100"
  (let ((ed-number (car (nterm-argument-to-list 1 0)))
        (ed-index 0)
        (ed-line (nterm-cursor-line-get))
        (ed-col (nterm-cursor-col-get)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "ED par=%d" ed-number))
    (while (< ed-index nterm-height)
      (cond
       ((< ed-index ed-line)
        (if (not (eq ed-number 0))
            (progn
              (nterm-cursor-position-set (cons ed-index 0))
              (nterm-vt100-blank-line ?\s ed-index nil t))))
       ((= ed-index ed-line)
        (cond
         ((eq ed-number 0)
          (nterm-cursor-position-set (cons ed-line ed-col))
          (nterm-vt100-blank-line ?\s ed-index (- nterm-width ed-col) t))
         ((eq ed-number 1)
          (nterm-cursor-position-set (cons ed-line 0))
          (nterm-vt100-blank-line ?\s ed-index (+ ed-col 1) t))
         (t ; ed-number = 2 and everything else
          (nterm-cursor-position-set (cons ed-index 0))
          (nterm-vt100-blank-line ?\s ed-index nil t))))
       ((> ed-index ed-line)
        (if (not (eq ed-number 1))
            (progn
              (nterm-cursor-position-set (cons ed-index 0))
              (nterm-vt100-blank-line ?\s ed-index nil t)))))
      (incf ed-index))
    ;; put the cursor back where it was
    (nterm-cursor-position-set (cons ed-line ed-col))
    (nterm-vt100-escape-end char)))

(defun nterm-vt100-el (char)
  "EL -- Erase In Line - host to vt100."
  (let ((el-number (car (nterm-argument-to-list 1 0)))
        (el-line (nterm-cursor-line-get))
        (el-col (nterm-cursor-col-get)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "EL par=%d" el-number))
    (cond
     ((eq el-number 0)
      (nterm-vt100-blank-line ?\s el-line (- nterm-width el-col) t))
     ((eq el-number 1)
      (nterm-cursor-col-set 0)
      (nterm-vt100-blank-line ?\s el-line (+ el-col 1) t))
     (t
      (nterm-vt100-blank-line ?\s el-line nil t)))
    (nterm-cursor-col-set el-col))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-escape-start (char)
  "Enter escape mode"
  (setq nterm-dispatch 'nterm-vt100-escape-dispatch))

(defun nterm-vt100-escape-end (char)
  "Exit escape mode"
  (setq nterm-argument "")
  (setcdr (assq 'wrap nterm-vt100-state) nil)
  (setq nterm-dispatch 'nterm-vt100-primary-dispatch))

(defun nterm-vt100-face-default ()
  "Set default term face."
  (list :foreground
        (car (nth (nterm-vt100-mode-decscnm-1) nterm-vt100-color))
        :background
        (car (nth (nterm-vt100-mode-decscnm-0) nterm-vt100-color))
        :slant 'normal
        :underline nil
        :strike-through nil))

(defun nterm-vt100-home ()
  "Home cursor.
Take in account of top margin with DECOM"
  (nterm-cursor-position-set
   (cons
    (if (nterm-vt100-mode-decom)
        (cdr (assq 'top-margin nterm-vt100-state))
      0) 0)))

(defun nterm-vt100-hts (char)
  "HTS -- Horizontal Tabulation Set - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "HTS"))
  (aset (cdr (assq 'tab nterm-vt100-state)) (nterm-cursor-col-get) t)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-hvp (char)
  "HVP -- Horizontal and Vertical Position - host to vt100
TBD handle DECOM"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "HVP"))
  (nterm-vt100-cup char))

;; What's the escape sequence for the following?
;; LNM -- Line Feed/New Line Mode

(defun nterm-vt100-ind (char)
  "IND -- Index - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "IND"))
  (nterm-vt100-lf char)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-key ()
  "Insert last typed char and send it to the process."
  (interactive)
  (if (symbolp last-input-event)
      (cond ((eq last-input-event 'return)
             (nterm-send-string "\r"))
            ((eq last-input-event 'backspace)
             (nterm-send-string ""))
            ((eq last-input-event 'escape)
             (nterm-send-string ""))
            ((eq last-input-event 'up)
             (if (nterm-vt100-mode-decckm)
                 (nterm-send-string "\eOA")
               (nterm-send-string "\e[A")))
            ((eq last-input-event 'down)
             (if (nterm-vt100-mode-decckm)
                 (nterm-send-string "\eOB")
               (nterm-send-string "\e[B")))
            ((eq last-input-event 'left)
             (if (nterm-vt100-mode-decckm)
                 (nterm-send-string "\eOD")
               (nterm-send-string "\e[D")))
            ((eq last-input-event 'right)
             (if (nterm-vt100-mode-decckm)
                 (nterm-send-string "\eOC")
               (nterm-send-string "\e[C")))
            ((eq last-input-event 'tab)
             (nterm-send-string "\t"))
            (t (nterm-vt100-keypad)))
    (let ((char (make-string 1 last-input-event)))
      (if (= last-input-event ?£)
          (nterm-send-string "#")
        (nterm-send-string char)))))

(defun nterm-vt100-keypad ()
  (let ((keypad-list nterm-vt100-keypad-table))
    (if (catch 'keypad-loop
          (while keypad-list
            (if (or (eq last-input-event
                        (intern (nth 0 (car keypad-list))))
                    (let ((keypad-numlock-off (nth 1 (car keypad-list))))
                      (if keypad-numlock-off
                          (eq last-input-event
                              (intern keypad-numlock-off)))))
                (throw 'keypad-loop t)
              (setq keypad-list (cdr keypad-list)))))
        (nterm-send-string (nth (if (nterm-vt100-mode-deckpam) 3 2)
                                (car keypad-list))))))

(defun nterm-vt100-init ()
  "Set vt100 modes to initial state"
  (setq nterm-vt100-mode
        (make-bool-vector (length nterm-vt100-mode-function) nil))
  (aset nterm-vt100-mode 2 t) ; DECAWM
  (setq nterm-vt100-mode-map
        (nterm-keymap
         'nterm-vt100-key
         '("<backpace>" "<up>" "<down>" "<left>" "<right>")
         nterm-vt100-keypad-table))
  (define-key nterm-vt100-mode-map (kbd "£") 'nterm-vt100-key)
  ;; reset state
  (setq nterm-vt100-state
        (list
         (cons 'face (nterm-vt100-face-default))
         ;; cons of (g0 . g1)
         (cons 'charset (list 'nterm-vt100-charset-us
                              'nterm-vt100-charset-us))
         (cons 'tab (make-bool-vector nterm-width nil))
         (cons 'top-margin 0)
         (cons 'bottom-margin (- nterm-height 1))
         (cons 'wrap nil)
         (cons 'attribute (nterm-mem-attribute))
         (cons 'blink-timer nil)
         (cons 'blink nil)
         ;; for save and restore cursor
         (cons 'save-charset (list 'nterm-vt100-charset-us
                                   'nterm-vt100-charset-us))
         (cons 'save-cursor (cons 0  0))
         (cons 'save-face (nterm-vt100-face-default))
         (cons 'save-attribute (nterm-mem-attribute))
         (cons 'save-char-table nterm-vt100-charset-us)
         ;; character table in use
         (cons 'char-table nterm-vt100-charset-us)))
  (nterm-vt100-tab-reset))

(defun nterm-vt100-lf (char)
  "Do a line feed, handle scrolling"
  (interactive)
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "LF"))
  (let ((lf-line (nterm-cursor-line-get)))
    (if (= lf-line (cdr (assq 'bottom-margin nterm-vt100-state)))
        (nterm-scroll-up
         (cdr (assq 'top-margin nterm-vt100-state))
         (cdr (assq 'bottom-margin nterm-vt100-state))
         'nterm-vt100-blank-line)
      (nterm-cursor-line-set (+ lf-line 1)))))

(defun nterm-vt100-line-draw (line &optional replace)
  "Draw LINE from terminal memory.
When replace is t, it replaces current line."
  (let ((draw-index 0)
        (draw-cur (nterm-cursor-position-get)))
    (nterm-cursor-position-set (cons line 0))
    (if replace
        (nterm-kill-line))
    (while (< draw-index nterm-width)
      (let ((draw-char
             (aref (cdr (assq 'char (nth line nterm-memory))) draw-index))
            (draw-attribute
             (aref (cdr (assq 'attr (nth line nterm-memory))) draw-index))
            (draw-face (nterm-vt100-face-default)))
        (if (aref draw-attribute nterm-vt100-char-underline)
            (nterm-face-underline draw-face))
        (if (and (aref draw-attribute nterm-vt100-char-blink)
                 (cdr (assq 'blink nterm-vt100-state)))
            (setq draw-face (nterm-vt100-face-default)
                  draw-char " "))
        (if (aref draw-attribute nterm-vt100-char-bright)
            (nterm-face-bright draw-face))
        (if (aref draw-attribute nterm-vt100-char-reverse)
            (nterm-face-reverse draw-face
                                (aref draw-attribute nterm-vt100-char-bright)))
        (insert draw-char)
        (remove-text-properties (- (point) 1) (point) '(face))
        (put-text-property (- (point) 1) (point) 'face draw-face)
        (incf draw-index)))
    (if (not (= line (- nterm-height 1)))
        (insert "\n"))
    (nterm-cursor-position-set draw-cur)))

(defun nterm-vt100-mode-decawm ()
  "Returns DECAWM mode"
  (aref nterm-vt100-mode 7))

(defun nterm-vt100-mode-decckm ()
  "Returns DECAWM mode"
  (aref nterm-vt100-mode 1))

(defun nterm-vt100-mode-decom ()
  "Returns DECOM mode"
  (aref nterm-vt100-mode 6))


(defun nterm-vt100-mode-deckpam ()
  (aref nterm-vt100-mode 10))

(defun nterm-vt100-mode-decscnm ()
  "Returns DECSCNM mode, 0 or 1 for caller's convenience"
  (aref nterm-vt100-mode 5))

(defun nterm-vt100-mode-decscnm-0 ()
  "Returns DECSCNM mode,
Returns 1 if set 0 otherwise"
  (if (aref nterm-vt100-mode 5) 1 0))

(defun nterm-vt100-mode-decscnm-1 ()
  "Returns DECSCNM mode,
Returns 0 if set 1 otherwise"
  (if (aref nterm-vt100-mode 5) 0 1))

(defun nterm-vt100-nel (char)
  "NEL -- Next Line - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "NEL"))
  (nterm-vt100-cr char)
  (nterm-vt100-lf char)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-parenthesis-open-start (char)
  (setq nterm-dispatch 'nterm-vt100-parenthesis-open-dispatch))

(defun nterm-vt100-parenthesis-close-start (char)
  (setq nterm-dispatch 'nterm-vt100-parenthesis-close-dispatch))

(defun nterm-vt100-question-start (char)
  "Esape bracket question mark dispatch"
  (setq nterm-dispatch 'nterm-vt100-question-dispatch))

(defun nterm-vt100-reset (char)
  "Fill the screen and memory with one character CHAR.
Home the cursor at the beginning."
  (nterm-blank-screen char)
  (nterm-vt100-init)
  (nterm-vt100-home))

(defun nterm-vt100-ri (char)
  "RI -- Reverse Index - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "RI"))
  (if (= (nterm-cursor-line-get) (cdr (assq 'top-margin nterm-vt100-state)))
      (nterm-scroll-down
       (cdr (assq 'top-margin nterm-vt100-state))
       (cdr (assq 'bottom-margin nterm-vt100-state))
       'nterm-vt100-blank-line)
    (nterm-vt100-cuu char))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-ris (char)
  "RIS -- Reset To Initial State - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "RIS"))
  (nterm-vt100-reset ?\s)
  (nterm-vt100-escape-end char))

(defun nterm-vt100-rm (char)
  "RM -- Reset Mode - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "RM"))
  (if (nterm-vt100-set-mode
       nterm-vt100-mode nterm-vt100-mode-function nil)
      (nterm-vt100-escape-end char)))

(defun nterm-vt100-scs-g0 (char)
  "SCS -- Select Character Set G0- host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "SCS %c" char))
  (setcar
   (cdr (assq 'charset nterm-vt100-state))
   (cond ((eq char ?A) 'nterm-vt100-charset-uk)
         ((eq char ?B) 'nterm-vt100-charset-us)
         ((eq char ?0) 'nterm-vt100-charset-special)
         ;; TBD Alternate Character ROM Standard Character Set
         ((eq char ?1) 'nterm-vt100-charset-us)
         ;; TBD Alternate Character ROM Special Graphics
         ((eq char ?2) 'nterm-vt100-charset-us)))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-scs-g1 (char)
  "SCS -- Select Character Set G0- host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "SCS %c" char))
  (setcar
   (cddr (assq 'charset nterm-vt100-state))
   (cond ((eq char ?A) 'nterm-vt100-charset-uk)
         ((eq char ?B) 'nterm-vt100-charset-us)
         ((eq char ?0) 'nterm-vt100-charset-special)
         ;; TBD Alternate Character ROM Standard Character Set
         ((eq char ?1) 'nterm-vt100-charset-us)
         ;; TBD Alternate Character ROM Special Graphics
         ((eq char ?2) 'nterm-vt100-charset-us)))
  (nterm-vt100-escape-end char))

(defun nterm-vt100-set-mode (mode function flag)
  "Set FLAG to MODE call function if mode has changed.
Functions are indexed in the vector FUNCTION.
Returns t unless a switch to vt52 has occurred."
  (let ((mode-list (nterm-argument-to-list 1 1))
        (mode-result t))
    (while mode-list
      (if (and (< (car mode-list) (length mode)))
          (let* ((mode-index (car mode-list))
                 (mode-function (aref function mode-index))
                 (mode-differ (not (eq (aref mode mode-index) flag))))
            (aset mode mode-index flag)
            (if (= mode-index 2) ; DECANM
                (setq mode-result nil))
            (if (and mode-function mode-differ)
                (funcall mode-function flag))))
      (setq mode-list (cdr mode-list)))
    mode-result))

(defun nterm-vt100-sgr (char)
  "SGR -- Select Graphic Rendition - host to vt100"
  (let ((sgr-list (nterm-argument-to-list 1 0))
        (sgr-attribute (cdr (assq 'attribute nterm-vt100-state)))
        (sgr-face (cdr (assq 'face nterm-vt100-state)))
        sgr-number)
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "SGR %S" sgr-list))
    (while sgr-list
      (setq sgr-number (car sgr-list))
      (cond
       ((eq sgr-number 0)
        (setq sgr-face (nterm-vt100-face-default))
        (aset sgr-attribute nterm-vt100-char-bright nil)
        (aset sgr-attribute nterm-vt100-char-underline nil)
        (aset sgr-attribute nterm-vt100-char-blink nil)
        (aset sgr-attribute nterm-vt100-char-reverse nil))
       ((eq sgr-number 1)
        (nterm-face-bright sgr-face)
        (aset sgr-attribute nterm-vt100-char-bright t))
       ((eq sgr-number 4)
        (nterm-face-underline sgr-face)
        (aset sgr-attribute nterm-vt100-char-underline t))
       ((eq sgr-number 5)
        (aset sgr-attribute nterm-vt100-char-blink t))
       ((eq sgr-number 7)
        (nterm-face-reverse sgr-face
                            (aref sgr-attribute nterm-vt100-char-bright))
        (aset sgr-attribute nterm-vt100-char-reverse t))
       (t (nterm-ansi-sgr sgr-number)))
      (setq sgr-list (cdr sgr-list)))
    (setcdr (assq 'face nterm-vt100-state) sgr-face)
    (nterm-vt100-escape-end char)))

(defun nterm-vt100-si (char)
  "Set G0 char table."
  (setcdr (assq 'char-table nterm-vt100-state)
          (eval (cadr (assq 'charset nterm-vt100-state)))))

(defun nterm-vt100-so (char)
  "Set G1 char table"
  (setcdr (assq 'char-table nterm-vt100-state)
          (eval (nth 2 (assq 'charset nterm-vt100-state)))))

(defun nterm-vt100-sm (char)
  "SM -- Set Mode - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "SM"))
  (if (nterm-vt100-set-mode
       nterm-vt100-mode nterm-vt100-mode-function t)
      (nterm-vt100-escape-end char)))

(defun nterm-vt100-switch ()
  "Switch to vt100."
  (use-local-map nterm-vt100-mode-map)
  (setq nterm-dispatch 'nterm-vt100-primary-dispatch))

(defun nterm-vt100-tab (char)
  "Move cursor to the next tab."
  (let ((tab-col (+ (nterm-cursor-col-get) 1))
        (target-col -1))
    (while (< tab-col nterm-width)
      (if (aref (cdr (assq 'tab nterm-vt100-state)) tab-col)
          (setq target-col tab-col
                tab-col nterm-width)) ; end loop
      (incf tab-col))
    (setq tab-col (if (= target-col -1) (- nterm-width 1) target-col))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "TAB going to col=%d" tab-col))
    (nterm-cursor-col-set tab-col)))

(defun nterm-vt100-tab-reset ()
  "Set bool vector with tabs set at every 8th character"
  (let ((nterm-index 1)
        (tabs (cdr (assq 'tab nterm-vt100-state))))
    (while (< nterm-index nterm-width)
      (if (eq (mod nterm-index 8) 0)
          (aset tabs nterm-index t)
        (aset tabs nterm-index nil))
      (incf nterm-index))))

(defun nterm-vt100-tab-clear ()
  "Clear all tabs
TBD use (make-vector nterm-width nil) instead"
  (let ((nterm-index 1)
        (tabs (cdr (assq 'tab nterm-vt100-state))))
    (while (< nterm-index nterm-width)
      (aset tabs nterm-index nil)
      (incf nterm-index))))


(defun nterm-vt100-tbc (char)
  "TBC -- Tabulation Clear - host to vt100"
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "TBC"))
  (let ((tbc-arg (car (nterm-argument-to-list 1 0)))
        (tabs (cdr (assq 'tab nterm-vt100-state))))
    (cond ((eq tbc-arg 0)
           (aset tabs (nterm-cursor-col-get) nil))
          ((eq tbc-arg 3) (nterm-vt100-tab-clear))))
  (nterm-vt100-escape-end char))

;;; VT52 emulator
(defvar nterm-vt52-state nil
  "State of the vt52 terminal")

(defvar nterm-vt52-charset-graphic
  (vconcat
   nterm-vt100-c0
   (let ((start ?\s)
         (end ?^)
         (index 0)
         (vec (make-vector 62 nil)))
     (while (< start end)
       (aset vec index start)
       (incf start)
       (incf index))
     vec)
   [?\s
    ?\s
    nil
    '("            "
      "            "
      "            "
      " ...        "
      "   .        "
      "   .        "
      "   .        "
      "   .        "
      "   .       ."
      "   .      . "
      " .....   .  "
      "         .  "
      "        .   "
      "       .    "
      "      .     "
      "      .     "
      "     .      "
      "    .       "
      "   .        "
      "   .        "
      "  .         "
      " .          "
      ".           "
      ".           ")
    '("            "
      "            "
      "            "
      "  ....      "
      "      .     "
      "      .     "
      "   ...      "
      "      .     "
      "      .    ."
      "      .   . "
      " .....   .  "
      "         .  "
      "        .   "
      "       .    "
      "      .     "
      "      .     "
      "     .      "
      "    .       "
      "   .        "
      "   .        "
      "  .         "
      " .          "
      ".           "
      ".           ")
    '("            "
      "            "
      "            "
      "  .....     "
      "  .         "
      "  .         "
      "  .....     "
      "      .     "
      "       .   ."
      "      .   . "
      "  .....  .  "
      "         .  "
      "        .   "
      "       .    "
      "      .     "
      "      .     "
      "     .      "
      "    .       "
      "   .        "
      "   .        "
      "  .         "
      " .          "
      ".           "
      ".           ")
    '("            "
      "            "
      "            "
      "  .....     "
      "      .     "
      "      .     "
      "      .     "
      "     .      "
      "     .     ."
      "    .     . "
      "    .    .  "
      "         .  "
      "        .   "
      "       .    "
      "      .     "
      "      .     "
      "     .      "
      "    .       "
      "   .        "
      "   .        "
      "  .         "
      " .          "
      ".           "
      ".           ")
    ?°
    ?±
    ?→
    ?…
    ?/
    ?↓
    '("............"
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            ")
    '("            "
      "            "
      "............"
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            ")
    '("            "
      "            "
      "            "
      "            "
      "            "
      "............"
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            ")
    '("            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "............"
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            ")
    '("            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "............"
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            ")
    '("            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "............"
      "            "
      "            "
      "            "
      "            "
      "            ")
    '("            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "............"
      "            "
      "            ")
    '("            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "            "
      "............")
    ?₀
    ?₁
    ?₂
    ?₃
    ?₄
    ?₅
    ?₆
    ?₇
    ?₈
    ?₉
    ?჻
    ])
  "VT52 graphic character set")

(defvar nterm-vt52-keypad-table
  ;; numlock on, numlock off, numeric, application
  '(("kp-0"       "kp-insert"  "0"    "\e?p")
    ("kp-1"       "kp-end"     "1"    "\e?q")
    ("kp-2"       "kp-down"    "2"    "\e?r")
    ("kp-3"       "kp-next"    "3"    "\e?s")
    ("kp-4"       "kp-left"    "4"    "\e?t")
    ("kp-5"       "kp-begin"   "5"    "\e?u")
    ("kp-6"       "kp-right"   "6"    "\e?v")
    ("kp-7"       "kp-home"    "7"    "\e?w")
    ("kp-8"       "kp-up"      "8"    "\e?x")
    ("kp-9"       "kp-prior"   "9"    "\e?y")
    ("kp-subtract" nil         "-"    "\e?m")
    ("kp-multiply" nil         ","    "\e?l")
    ("kp-decimal"  "kp-delete" "."    "\e?n")
    ("kp-enter"    nil         "\r"   "\e?M")
    ("f1"          nil         "\eP"  "\eP")
    ("f2"          nil         "\eQ"  "\eQ")
    ("f3"          nil         "\eR"  "\eR")
    ("f4"          nil         "\eS"  "\eS"))
  "Table of auxiliary keypad codes of a vt52.
Comma is mapped to the * multiply key. A PC keyboard doesn't have
a comma in the keypad. Function keys are mapped to the PC
function keys. If you have a different keyboard map these keys to
your liking.")

(defvar nterm-vt52-mode-map nil
  "Nterm keymap for vt52, use `nterm-mode-hook' to customize.")

(nterm-defdispatch ; Primary dispatch of a VT52
 '(nterm-vt52-primary-dispatch
   128 nterm-vt52-char-self
   ?\e nterm-vt52-escape-start
   ?\n nterm-vt100-lf
   ?\r nterm-vt100-cr
   ?\t nterm-vt100-tab
   ?\v nterm-vt100-lf))

(nterm-defdispatch ; Escape (ESC) dispatch of a vt52
 '(nterm-vt52-escape-dispatch
   128 nterm-vt52-escape-end
   ?A nterm-vt52-cursor-up
   ?B nterm-vt52-cursor-down
   ?C nterm-vt52-cursor-right
   ?D nterm-vt52-cursor-left
   ?F nterm-vt52-enter-graphics
   ?G nterm-vt52-exit-graphics
   ?H nterm-vt52-home
   ?I nterm-vt52-reverse-line-feed
   ?J nterm-vt52-erase-to-end-of-screen
   ?K nterm-vt52-erase-to-end-of-line
   ?Y nterm-vt52-escape-y-start
   ?Z nterm-vt52-identify
   ?= nterm-vt52-enter-alternate-keypad
   ?> nterm-vt52-exit-alternate-keypad
   ?< nterm-vt52-enter-ansi-mode))

(nterm-defdispatch ; Escape y (ESC y) dispatch line
 '(nterm-vt52-escape-y-dispatch-line
   128 nterm-vt52-escape-end))

(nterm-defdispatch ; Escape y (ESC y) dispatch col
 '(nterm-vt52-escape-y-dispatch-col
   128 nterm-vt52-escape-end))


(let ((y-index 32)) ; fixup dispatch line
  (while (< y-index 56)
    (aset nterm-vt52-escape-y-dispatch-line
          y-index 'nterm-vt52-escape-y-line)
    (incf y-index)))

(let ((y-index 40)) ; fixup dispatch col
  (while (< y-index 120)
    (aset nterm-vt52-escape-y-dispatch-col
          y-index 'nterm-vt52-escape-y-col)
    (incf y-index)))

(defun nterm-vt52-blank-line (char)
  "Insert a new line of CHAR till the end of the line.
The cursor is left at the end of the line."
  (let ((blank-length (- nterm-width (nterm-cursor-col-get)))
        (blank-index 0))
    (while (< blank-index blank-length)
      (nterm-vt52-char-self char)
      (incf blank-index))))

(defun nterm-vt52-char-self (char)
  "Insert character."
  (if (= (point) (point-max))
      (goto-char (- (point) 1)))
  (let* ((insert-line (nterm-cursor-line-get))
         (insert-col (nterm-cursor-col-get))
         (insert-mem-char (cdr (assq 'char (nth insert-line nterm-memory))))
         (insert-char-table (cdr (assq 'charset nterm-vt52-state)))
         (insert-char (if (< char (length insert-char-table))
                          (aref insert-char-table char))))
    (if insert-char
        (progn
          (aset insert-mem-char insert-col insert-char)
          (delete-char 1)
          (insert (char-to-string insert-char)))))
  (if (= (point) (line-end-position))
      (if (= (point) (point-max))
          (progn
            (nterm-scroll-up 0 nterm-height 'nterm-vt52-blank-line)
            (nterm-cursor-col-set 0)))
    (nterm-cursor-position-set
     (cons (+ (nterm-cursor-line-get) 1) 0))))

(defun nterm-vt52-cursor-down (char)
  "Cursor Down -- host to vt52."
  (let ((cud-line (+ (nterm-cursor-line-get) 1)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "vt52 cursor down"))
    (if (< cud-line nterm-height)
        (nterm-cursor-line-set cud-line)))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-cursor-left (char)
  "CUB -- Cursor Backward -- host to vt52"
  (let ((cub-col (- (nterm-cursor-col-get) 1)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "vt52 cursor left"))
    (if (>= cub-col 0)
        (nterm-cursor-col-set cub-col)))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-cursor-right (char)
  "Cursor Right -- host to vt52."
  (let ((cuf-col (+ (nterm-cursor-col-get) 1)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "vt52 cursor right"))
    (if (< cuf-col nterm-width)
        (nterm-cursor-col-set cuf-col)))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-cursor-up (char)
  "Cursor Up -- host to vt52."
  (let ((cuu-line (- (nterm-cursor-line-get) 1)))
    (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
        (message "vt52 cursor up"))
    (if (<= cuu-line 0)
        (nterm-cursor-line-set cuu-line)))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-enter-graphics (char)
  "Enter graphics mode -- host to vt52."
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "vt52 enter graphics mode"))
  (setcdr (assq 'charset nterm-vt52-state) nterm-vt52-charset-graphic)
  (nterm-vt52-escape-end char))

(defun nterm-vt52-erase-to-end-of-screen (char)
  (let ((end-line (nterm-cursor-line-get))
        (end-position (nterm-cursor-position-get)))
    (if (= (logand nterm-debug nterm-debug-vt52) nterm-debug-vt52)
        (message "vt 52 erase to end of screen"))
    (nterm-vt52-blank-line ?\s)
    (incf end-line)
    (while (< end-line nterm-height)
      (nterm-cursor-position-set (cons end-line 0))
      (nterm-vt52-blank-line ?\s)
      (incf end-line))
    (nterm-cursor-position-set end-position))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-erase-to-end-of-line (char)
  (if (= (logand nterm-debug nterm-debug-vt52) nterm-debug-vt52)
      (message "vt 52 erase to end of line"))
  (nterm-vt52-blank-line ?\s)
;; TBD   ?\s (nterm-cursor-line-get)  (- nterm-width (nterm-cursor-col-get)))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-escape-end (char)
  "Exit escape."
  (setq nterm-dispatch 'nterm-vt52-primary-dispatch))

(defun nterm-vt52-escape-start (char)
  "Start escape"
  (setq nterm-dispatch 'nterm-vt52-escape-dispatch))
(defun nterm-vt52-escape-y-col (char)
  "Set column - host to vt52."
  (if (= (logand nterm-debug nterm-debug-vt52) nterm-debug-vt52)
      (message "vt 52 ESC y set col"))
  (nterm-cursor-col-set (- char 32))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-escape-y-line (char)
  "Set line -- host to vt52"
  (if (= (logand nterm-debug nterm-debug-vt52) nterm-debug-vt52)
      (message "vt 52 ESC y set line"))
  (nterm-cursor-line-set (- char 32))
  (setq nterm-dispatch 'nterm-vt52-escape-y-dispatch-col))

(defun nterm-vt52-escape-y-start (char)
  "Setup ESC y dispatch -- host to vt52"
  (setq nterm-dispatch 'nterm-vt52-escape-y-dispatch-line))

(defun nterm-vt52-exit-graphics (char)
  "Exit graphics mode -- host to vt52."
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt100)
      (message "vt52 exit graphics mode"))
  (setcdr (assq 'charset nterm-vt52-state) nterm-vt100-charset-us)
  (nterm-vt52-escape-end char))

(defun nterm-vt52-home (char)
  "Cursor to home -- host to vt52."
  (nterm-cursor-position-set (cons 0 0))
  (nterm-vt52-escape-end char))

(defun nterm-vt52-init ()
  "Initialise vt52."
  (setq nterm-vt52-state
        (list
         (cons 'application-keypad nil)
         (cons 'charset nterm-vt100-charset-us)))
  (setq nterm-vt52-mode-map
        (nterm-keymap
         'nterm-vt100-key
         '("<backpace>" "<up>" "<down>" "<left>" "<right>")
         nterm-vt100-keypad-table)))

(defun nterm-vt52-reverse-line-feed (char)
  "Reverse Line Feed -- host to vt52."
  (if (= (logand nterm-debug nterm-debug-vt100) nterm-debug-vt52)
      (message "vt52 reverse line feed "))
  (if (= (nterm-cursor-line-get) 0)
      (nterm-scroll-down 0 nterm-height 'nterm-vt52-blank-line)
    (nterm-vt52-cursor-up char))
  (nterm-vt100-escape-end char))

(defun nterm-vt52-switch ()
  (setq nterm-dispatch 'nterm-vt52-primary-dispatch)
  (use-local-map nterm-vt52-mode-map))

(defun nterm-vt52-identify ()
  "Identify -- host to vt52"
  (if (= (logand nterm-debug nterm-debug-vt52) nterm-debug-vt52)
      (message "vt 52 identify"))
  (nterm-send-string "\e/Z"))

(defun nterm-vt52-enter-alternate-keypad ()
  "Enter alternate keypad -- host to vt52."
  (setcdr (assq 'application-keypad nterm-vt52-state) t))

(defun nterm-vt52-exit-alternate-keypad ()
  (setcdr (assq 'application-keypad nterm-vt52-state) nil))

(defun nterm-vt52-enter-ansi-mode ()
  "Enter ANSI mode -- host to vt52.
TBD")


;;; Memory dump
(defvar nterm-mem-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'kill-this-buffer)
    (define-key map "t" 'nterm-mem-debug-char-toggle)
    map)
  "Keymap of the nterm memory dump mode")

(defvar nterm-mem-buffer "*nterm mem*"
  "Name of the buffer where to dump memory")

(defvar nterm-mem-time 5
  "Time in second that memory dump will refresh")

(defvar nterm-mem-debug-char t
  "Display char when set to t, hex when nil")
(defun nterm-mem-debug-header ()
  "Header of the memory dumper."
  (let ((mem-col 0))
    (insert "TAB")
    (while (< mem-col nterm-width)
      (insert " ")
      (insert
       (if (aref (cdr (assq 'tab nterm-vt100-state)) mem-col) "*" " "))
      (incf mem-col))
    (insert "\nCoA")
    (setq mem-col 0)
    (while (< mem-col nterm-width)
      (insert (format "%02d" mem-col))
      (incf mem-col))
    (insert "\n")))

(defun nterm-mem-dump ()
  "Dump memory content."
  (let ((point (point)))
    (erase-buffer)
    (nterm-mem-debug-header)
    (let ((dump-line 0))
      (while (< dump-line nterm-height)
        ;; first col displays line number followed by line attribute
        (insert
         (format
          "%02d%1x"
          (+ dump-line 1)
          (nterm-mem-vector-to-dec
           (cdr (assq 'line-attr (nth dump-line nterm-memory))))))
        (nterm-mem-dump-line dump-line 'char "C")
        (nterm-mem-dump-line dump-line 'attr "   A")
        (incf dump-line)))
    (goto-char point)))

(defun nterm-mem-debug-char-toggle ()
  "Toggle between hex and char display."
  (interactive)
  (setq nterm-mem-debug-char (not nterm-mem-debug-char)))

(defun nterm-mem-dump-line (line what begin)
  "Dump a LINE of WHAT from memory, prefix line with BEGIN."
  (let ((dump-col 0))
    (insert begin)
    (while (< dump-col nterm-width)
      (let ((dump-cell (aref (cdr (assq what (nth line nterm-memory)))
                             dump-col)))
        (insert
         (cond
          ((eq what 'attr)
           (format "%02x" (nterm-mem-vector-to-dec dump-cell)))
          ((eq what 'char)
           (format (if nterm-mem-debug-char " %c" "%2x") dump-cell))
          (t (error "Wrong what %S" what))))
        (incf dump-col)))
    (insert "\n")))

(defun nterm-mem-mode ()
  (kill-all-local-variables)
  (setq
   truncate-lines t
   major-mode 'nterm-mem-mode
   mode-name "nterm memory dump")
  (use-local-map nterm-mem-mode-map))

(defun nterm-mem ()
  "Run the memory display."
  (interactive)
  (run-at-time nil nterm-mem-time 'nterm-mem-display-timer)
  (with-current-buffer (get-buffer-create nterm-mem-buffer)
    (nterm-mem-dump))
  (pop-to-buffer nterm-mem-buffer)
  (nterm-mem-mode))


(defun nterm-mem-attribute ()
  "Return a cell of attribute."
  (make-bool-vector 4 nil))

(defun nterm-mem-display-timer ()
  (if (get-buffer nterm-mem-buffer)
      (with-current-buffer (get-buffer nterm-mem-buffer)
        (nterm-mem-dump))
    ;; cancel timer
    (let ((list timer-list))
      (while list
        (let ((elt (pop list)))
          (when (equal (symbol-name (aref elt 5)) "nterm-mem-display-timer")
            (cancel-timer elt)))))))

(defun nterm-mem-line (char)
  "Return a  memory line filled with CHAR."
  (list (cons 'attr (make-vector nterm-width (nterm-mem-attribute)))
        (cons 'char (make-vector nterm-width char))
        (cons 'line-attr (make-bool-vector 2 nil))))

(defun nterm-mem-vector-to-dec (vector)
  (let ((vector-index 0)
        (vector-base 1)
        (vector-length (length vector))
        (vector-result 0))
    (while (< vector-index vector-length)
      (if (aref vector vector-index)
          (setq vector-result (+ vector-result vector-base)))
      (setq vector-base (* vector-base 2))
      (incf vector-index))
    vector-result))

;; Recorder
;;; Recorder
(defvar nterm-record-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'kill-this-buffer)
    (define-key map "s" 'nterm-record-step)
    (define-key map "t" 'nterm-record-toggle)
    map)
  "Keymap of the nterm memory dump mode")

(defvar nterm-record-buffer "*nterm record*"
  "Buffere where recording happens")

(defvar nterm-record-col 0
  "Column number of the recorder")

(defvar nterm-record-max-col 60
  "Number of columnt where recorder goes to the next line")

(defun nterm-record ()
  (interactive)
  (get-buffer-create nterm-record-buffer)
  (pop-to-buffer nterm-record-buffer)
  (setq nterm-record-enable t)
  (nterm-record-mode))

(defun nterm-record-mode ()
  (kill-all-local-variables)
  (setq
   major-mode 'nterm-record-mode
   mode-name "nterm record"
   truncate-lines t)
  (use-local-map nterm-record-mode-map))

(defun nterm-record-insert (string)
  "Append STRING to the record buffer"
  (let ((record-length (length string))
        (record-index 0)
        (record-point (point))
        (record-buf (current-buffer))
        record-char)
    (set-buffer nterm-record-buffer)
    (goto-char (point-max))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert string)
      (append-to-file (point-min) (point-max) "~/pipe"))
    (while (< record-index record-length)
      (let ((char (aref string record-index)))
        (insert (if (eq char ?\n)
                    ?\x01
                  char)))
      (incf nterm-record-col)
      (if (= nterm-record-col nterm-record-max-col)
          (progn
            (insert "\n")
            (setq nterm-record-col 0)))
      (incf record-index))
    (goto-char record-point)
    (set-buffer record-buf)))


(defun nterm-record-step ()
  (interactive)
  (let* ((send-begin (line-beginning-position))
         (send-end (line-end-position))
         (send-string (buffer-substring-no-properties send-begin send-end))
         (send-length (length send-string))
         (send-index 0)
         (send-win (selected-window)))
    (while (< send-index send-length)
      (if (eq (aref send-string send-index) ?\x01)
          (aset send-string send-index ?\n))
      (incf send-index))
    (nterm-emulate nil send-string)
    ;; stuff line to a named pipe, useful to compare with xterm
    ;; create the pipe with mkfifo ~/pipe
    ;; run the following command in xterm
    ;; while true; do cat < ~/pipe; done
    (if (file-exists-p "~/pipe")
        (with-temp-buffer
          (insert send-string)
          (append-to-file (point-min) (point-max) "~/pipe")))
    (forward-line 1)))

(defun nterm-record-toggle ()
  (interactive)
  (if (setq nterm-record-enable (not nterm-record-enable))
      (message "Recording on")
    (message "Recording off")))

(provide 'nterm)

;;; Copyright crap
;; Local Variables:
;; compile-command: "make"
;; End:

;; Copyright (C) 2009 Ivan Kanis
;; Author: Ivan Kanis
;; $Id$
;;
;; This program is free software ; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation ; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY ; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; nterm.el ends here
