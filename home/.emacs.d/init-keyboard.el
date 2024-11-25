;;; init.el.keyboard.el -- included by init.el
;;
;;; Commentary:
;;
;; Keymap Lookup Order:
;; (define-key KEYMAP KEY DEF)
;; 1. keymap overriding-terminal-local-map is for terminal-specific key binds.
;; 2. keymap overriding-local-map is for keys that override all other local keymaps.
;; 3. keymap char property at point is for keymaps that are local to the character point is at.
;; 4. keymap emulation-mode-map-alists is for advanced multi-mode keymap management.
;; 5. keymap minor-mode-overriding-map-alist is for overriding the keymaps used by minor modes in major modes.
;; 6. keymap minor-mode-map-alist is the preferred means of specifying the keymaps for minor modes.
;; 7. keymap text property at point is like the one above for char properties but is for text properties only.
;; 8. (current-local-map) is for keymaps defined in the buffers’ current local map.
;;    `(local-set-key KEY COMMAND)'
;; 9. (current-global-map) is the last place Emacs will look for key binds and it is for the global ones.
;;    (global-set-key KEY COMMAND)
;;
;; Uploaded at https://github.com/paulihano/nim-switch-buffers
;;
;;; Code:

;;;;;;;;;;;;;;;;
;; nim-mode uses emulation-mode-map.
;;   give high precedence to key declarations.
;;   override any other minor mode.
;;;;;;;;;;;;;;;;

;; Main use is to have my key bindings have the highest priority
(defvar nim-mode-map (make-sparse-keymap)
  "Keymap for nim-mode assigned via `bind-keys' at the end of this file.")

(define-minor-mode nim-mode
  "A minor mode to override major mode keybindings."
  :init-value t ;; needed to get enabled in `fundamental-mode'
  :lighter " nim"
  :keymap nim-mode-map)
(provide 'nim-mode) ; add `nim-mode' to global `features'
(define-globalized-minor-mode global-nim-mode nim-mode
  (lambda()(nim-mode t))) ; turn on in every buffer
(add-to-list 'emulation-mode-map-alists ; high precedence
             `((nim-mode . ,nim-mode-map)))

;;;;;;;;;;;;;;;;;;
;; REPLACE NIM-RING WITH NEW TAB-MODE
;; tab-bar-mode : per frame, tab represents a window
;;   uses header line on top of each window (below menu bar and above tool bar)
;;     tab-bar-mode toggle mode (may customize to set startup value)
;;     use tabbar-local-mode to retrieve tabbar if overwritten by mode
;;   tabs are grouped by major mode
;;   hide : show only current group (page of tab set)
;; tab-line-mode : per window, each tab represents a buffer (displayed on bottom)
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; -- used to order buffers -- primitive reading
;; ring - a cons cell link list which assumed to form a ring
;; a ring has no beginning for end, but is referenced by a current cons
;;;;;;;;;;;;;;;;

(defun nim-ring-relative (DELTA RING)
  "Return the cons DELTA relative to the ring RING list."
  (unless (integerp DELTA)
    (error "Expecting integerp DELTA but found %s" DELTA))
  (if (or (not RING) (not DELTA))
      RING ;; done
    (while (< 0 DELTA)
      (setq RING (cdr RING))
      (setq DELTA (1- DELTA)))
    (if (eq 0 DELTA)
        RING ;; done
      (let ((zFirst RING)
            (zRelative RING))
        (while (> 0 DELTA)
          (setq RING (cdr RING))
          (setq DELTA (1+ DELTA)))
        (while (not (eq RING zFirst))
          (unless RING (error "Ring has end!"))
          (setq RING (cdr RING))
          (setq zRelative (cdr zRelative)))
        zRelative)))) ;; done

(defun nim-ring-relative-moved (DELTA RING &optional MAKE-LIST)
  "Copy RING moving current cons DELTA distance around the ring.
First member of returned ring will have the same car.
If MAKE-LIST then leave nil end."
  (unless (integerp DELTA)
    (error "Expecting integerp DELTA but found %s" DELTA))
  (when RING
    (let ((zStop (nim-ring-relative
                  (if (> 0 DELTA) (1- DELTA) DELTA) RING)))
      (let* ((the-end (cons (car RING) nil))
             (zStart the-end)
             (zRing (cdr zStop)))
        (while (not (eq zRing zStop))
          (unless zRing (error "Ring has end!"))
          (when (not (eq zRing RING))
            (setq the-end (setcdr the-end (cons (car zRing) nil))))
          (setq zRing (cdr zRing)))
        (if MAKE-LIST zStart (setcdr the-end zStart))))))

(defun nim-ring-with (RING FILTER &optional MAKE-LIST)
  "Copy RING with members when FILTER(car).
If MAKE-LIST then leave nil end."
  (unless (functionp FILTER)
    (error "Expecting functionp FILTER but found %s" FILTER))
  (when RING
    (let* (the-end
           zStart
           (zRing RING))
      (while (progn
               (unless zRing (error "Ring has end!"))
               (when (funcall FILTER (car zRing))
                 (if the-end
                     (setq the-end (setcdr the-end (cons (car zRing) nil)))
                   (setq the-end (setq zStart (cons (car zRing) nil)))))
               (not (eq (setq zRing (cdr zRing)) RING))))
      (if MAKE-LIST zStart (setcdr the-end zStart)))))

(defvar nim-ring-next-match-default-test
  (lambda ( CAR AGAINST ) (eq CAR AGAINST))
  "Default test function for nim-ring-next-match.")

(defun nim-ring-next-match (AGAINST RING &optional TEST)
  "Return existing ring whose TEST(car AGAINST) or nil if not found.
Always considers TEST(car RING) first."
  (if TEST (unless (functionp TEST)
             (error "Expecting functionp TEST but found %s" TEST))
    (setq TEST nim-ring-next-match-default-test))
  (when RING
    (catch 'return
      (let ((zRing RING))
        (while (progn
                 (unless zRing (error "Ring has end!"))
                 (when (funcall TEST (car zRing) AGAINST)
                   (throw 'return zRing))
                 (not (eq (setq zRing (cdr zRing)) RING))))) nil)))

;;;;;;;;;;;;;;;; -- used to order buffers -- primitive modification
;; ringset - a ring whose car of each cons is unique
;; a ringset is a ring which inserts cars only with nim-ringset-add-car
;;;;;;;;;;;;;;;;

(defun nim-ringset-with-car (NEW-CAR RING &optional TEST)
  "When not member, create new ring with NEW-CAR inserted before RING.
Otherwise return existing member.
Use TEST to determine if NEW-CAR matches (car RING).
Default TEST to (lambda(CAR) (eq CAR NEW-CAR))."
  (let ((zNewCar NEW-CAR)) ; dynamic binding for default TEST
    (when TEST (unless (functionp TEST)
                 (error "Expecting functionp TEST but found %s" TEST)))
    (let ((zFound (nim-ring-next-match NEW-CAR RING TEST)))
      (if zFound zFound ; no copy
        (let* ((zNew (cons NEW-CAR nil))
               (the-end zNew))
          (when RING
            (let ((zRing RING))
              (while (progn
                       (unless zRing (error "Ring has end!"))
                       (setq the-end (setcdr the-end (cons (car zRing) nil)))
                       (not (eq (setq zRing (cdr zRing)) RING))))))
          (setcdr the-end zNew))))))

;;;;;;;;;;;;;;;; -- used during window closing judge automatic buffer killing --
;; mortal - mortal buffers are to be killed after closing its window
;; enforced by keystroke declarations
;;;;;;;;;;;;;;;;

(defun nim-buffer-filep (&optional BUFF)
  "Is BUFF (default `current-buffer') assocated with a file?"
  (buffer-file-name BUFF)) ; frames?

(defun nim-buffer-diredp (&optional BUFF)
  "Is BUFF (default `current-buffer') `major-mode' \"dired-mode\"?"
  (equal "dired-mode"
         (if BUFF (with-current-buffer BUFF major-mode) major-mode)))

(defun nim-buffer-displayp (&optional BUFF)
  "Is BUFF (default `current-buffer') is displayed in a window?"
  (get-buffer-window BUFF))

(defun nim-buffer-killedp (&optional BUFF)
  "Has BUFF (default `current-buffer') been killed?"
  (not (buffer-name BUFF)))

(defun nim-buffer-minip (&optional BUFF)
  "Is BUFF (default `current-buffer') the mini-buffer?"
  (minibufferp BUFF))

(defun nim-buffer-scratchp (&optional BUFF)
  "Is BUFF (default `current-buffer') the scratch buffer?"
  (string-equal "*scratch*"
                (buffer-name (or BUFF (current-buffer)))))

(defun nim-buffer-unsavedp (&optional BUFF)
  "Is BUFF (default `current-buffer') an unititled buffer?
This test is based upon a title match which is part of this code."
  (string-match "^\\*Unsaved\\*\\(<[0-9]>\\)?$"
                (buffer-name BUFF)))

(defun nim-buffer-mortalp (&optional BUFF)
  "Should BUFF be killed after window is killed?"
  (not (or (nim-buffer-filep BUFF); is associated with a file
           (nim-buffer-displayp BUFF) ; is displayed in a window
           (nim-buffer-minip BUFF) ; is the minibuffer
           (nim-buffer-scratchp BUFF) ; is the scratch buffer
           (nim-buffer-unsavedp BUFF)))) ; is unsaved

;;;;;;;;;;;;;;;;;; -- used to order buffers -- filter
;; buffer-type - used to determine buffers that are regularly visited vs not
;; enforced by keystroke declarations
;;;;;;;;;;;;;;;;

(defcustom nim-buffer-names-unwanted
  '("*Flycheck error messages*" ; internal error - cannot become current
    "*Messages*" ; perpetual
    )
  "Buffer names understood by `nim-buffer-wanted' as being unwanted.
The default may be unwanted, but this is for assurance if default changes."
  :group 'string)

(defcustom nim-buffer-names-wanted
  '("*Packages*" "*scratch*") ; list-packages
  "Buffer names understood by `nim-buffer-wanted' as being unwanted."
  :group 'string)

(defun nim-buffer-wantedp (&optional BUFF)
  "Should buffer BUFF be included by `nim-buffer-ringset'?"
  (let ((zName (buffer-name BUFF)))
    (cond ((not (buffer-name BUFF)) ; killed buffer
           nil)
          ((or (nim-buffer-filep BUFF)
               (nim-buffer-diredp BUFF)
               (nim-buffer-unsavedp BUFF)
               (member zName nim-buffer-names-wanted))
           t)
          ((or (string-equal " " (substring zName 0 1))0
               (eq buffer-saved-size 0)
               (member zName nim-buffer-names-unwanted))
           nil)
          ((string-equal "*" (substring zName 0 1))
           t)
          (t nil)))) ; unfortunately, to assume t is risky

;;;;;;;;;;;;;;;;;; -- used to order buffers -- high level access
;; buffer-ringset - used to determine apparent buffer order
;; enforced by keystroke declarations
;;;;;;;;;;;;;;;;

(defvar _nim-buffer-ringset '()
  "The last known ring of wanted buffers.
Use `nim-buffer-ringset' for a updated buffer ringset.")
(setq _nim-buffer-ringset nil) ;; rebuild malformed ringset

(defun nim-buffer-ringset (&optional BUFF)
  "Verify wanted buffers in ordered ringset.
The car value of the result will be BUFF or `current-buffer'.
Filters unwanted buffers and inserts wanted buffers.
Return value is stored in `_nim-buffer-ringset'."
  (let ((zRing (nim-ring-with _nim-buffer-ringset #'nim-buffer-wantedp)))
    ;; insert wanted
    (dolist (zBuff (buffer-list))
      (when (nim-buffer-wantedp zBuff)
        (setq zRing (nim-ringset-with-car zBuff zRing))))
    ;; assign current-buffer
    (if BUFF (unless (bufferp BUFF)
               (error "Expecting bufferp BUFF but found %s" BUFF))
      (setq BUFF (current-buffer)))
    (setq _nim-buffer-ringset (nim-ringset-with-car BUFF zRing))))

(defun nim-buffer (&optional DELTA BUFF)
  "Return the buffer DELTA relative to BUFF.
Use this to determine new buffer during [C-next] and [C-prior].
Stores cons holding result in '_nim-buffer-ringset'."
  (car (setq _nim-buffer-ringset (nim-ring-relative DELTA (nim-buffer-ringset BUFF)))))

(defun nim-buffer-move (DELTA &optional BUFF)
  "Shift BUFF in `nim-buffer-ringset' DELTA relative to its current position.
Used to rearrange buffer order during [C-S-next] and [C-S-prior]."
  (setq _nim-buffer-ringset
        (nim-ring-relative-moved DELTA (nim-buffer-ringset BUFF))))

;;;;;;;;;;;;;;;;
;; singleton routines used by keystroke declarations
;;;;;;;;;;;;;;;;

(defun nim-binary-toggle ()
  "Rotate between mixed-octal (normal), mixed-hex, and hexl."
  (interactive)
  (cond
   ((and (not (eq 'hexl-mode major-mode))
         (not buffer-display-table))
    (hexl-mode)
    (message "(hexl-mode)"))
   ((eq 'hexl-mode major-mode)
    (hexl-mode-exit)
    (defvar mixed-hex-display-table
      (progn
        (let ((zTable (make-display-table)))
          (cl-loop
           for x in
           (append (number-sequence 127 255)
                   (number-sequence 0 8)
                   (number-sequence 11 31))
           do (aset zTable (unibyte-char-to-multibyte x)
                    (cl-map 'vector
                            (lambda (CHAR) (make-glyph-code CHAR 'escape-glyph))
                            (format "\\%02x" x))))
          zTable))
      "A display-table showing unusual ASCII characters as hex values.")
    (setq buffer-display-table mixed-hex-display-table)
    (message "(setq buffer-display-table mixed-hex-display-table)"))
   (t
    (setq buffer-display-table nil)
    (message "(setq buffer-display-table nil) ; standard-display-table uses octal"))))

(defun nim-bol ( POINT )
  "Toggle point to logical|physical start of line if different.
If same, toggle select-all|saved-point !!! NOT CODED !!!.
Shift extends point selection.
POINT from interactive d."
  (interactive "^d")
  (back-to-indentation)
  (when (eq POINT (point))
    (if (bolp)
        (if (not (use-region-p))
            (mark-whole-buffer)
            (pop-mark)
            (goto-char (mark))))
    (beginning-of-visual-line)))

(defun nim-delete ( &optional APPENDP )
  "`kill-region' or `delete-char'.
Append deleted if to clipboard APPENDP.
!!! Trouble operating properly during `cua-rectangle-mark-mode' !!!
!!! Want ability to forward a keystroke onward down the keyboard tree !!!"
  (interactive)
  (cond ((and (boundp rectangle-mark-mode) rectangle-mark-mode)
         (message "active rectangle-mark-mode") ;; does NOT appear when it should
         (cua-delete-region))
        (APPENDP
           (when (or (use-region-p) (not (eobp)))
             (append-next-kill)
             (kill-region
              (if (use-region-p) (region-beginning) (point))
              (if (use-region-p) (region-end) (+ 1 (point))))))
        ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        (t (delete-char 1)))) ;; forget singly removed characters without APPENDP

(defun nim-error-next( N &optional RECURSE ) ; C-n
  "Previous linter error where N is count (usually 1 or -1).
RECURSE indicates that function was recursively called."
  (interactive)
  (cond
   ((and (boundp 'flycheck-mode) flycheck-mode)
    (flycheck-next-error N))
   ((and (boundp 'flymake-mode) flymake-mode)
    (flymake-goto-next-error N))
   ((and (not RECURSE)
         (boundp 'flycheck-mode)) ; flycheck is faster
    (flycheck-mode)
    (nim-error-next N t))
   ((and (not RECURSE)
         (boundp 'flymake-mode))
    (flymake-mode)
    (unless RECURSE (nim-error-next N t)))
   (t (message "flycheck or flymake not available"))))

(defun nim-eol ()
  "Allow shift selection moving to wrapped eol then physical eol."
  (interactive "^")
  (if truncate-lines
      (end-of-visible-line)
    (let ((the-point (point)))
      (end-of-visual-line)
      (when (equal the-point (point))
        (end-of-visible-line)
        (when (equal the-point (point))
          (end-of-visual-line 2))))))

(defun nim-eval ( PREFIX )
  "`eval-last-sexp' if possible (forwarding PREFIX).
`eval-buffer' with `message' if blank line."
  (interactive "P")
  (cond ((save-excursion
           (beginning-of-line)
           (looking-at-p "[[:blank:]]*$"))
         (message "eval-buffer %s" (current-buffer))
         (eval-buffer))
        ((eq ?\) (char-before))
         (eval-last-sexp PREFIX))
        ((eq ?\( (char-after))
         (let ((FROM (point))
               (TO))
           (save-excursion
             (forward-sexp)
             (setq TO (point)))
           (message (buffer-substring-no-properties FROM TO))
           (eval-sexp-add-defvars (buffer-substring-no-properties FROM TO))
           (eval-region FROM TO)))
        (t (message "nim-eval expects mark at end of sexpression to evaluate"))))

(defun nim-find ( PREFIX ) ; C-f
  "Begin/end searching.
If mark is in iedit selection, toggle selection inclusion.
Else if iedit-mode, toggle visibility of non-matching lines
showing 'PREFIX' lines above and below matches (default 2).
Ohtherwise begin `isearch-mode' with selected region
or prompt for search string."
  (interactive "P")
  ;; i-edit font-lock issue causes collusion between i-edit region and emacs region.
  ;; i-edit mode needs turned off and on to eliminate selection collusion issues.
  (cond ((and (boundp 'iedit-mode) iedit-mode)
         (if (iedit-find-current-occurrence-overlay)
             (iedit-toggle-selection) ; (kbd "M-;")
           (defvar iedit-occurrence-context-lines)
           (if PREFIX
               (setq iedit-occurrence-context-lines PREFIX)
             (unless iedit-occurrence-context-lines
               (setq iedit-occurrence-context-lines 2)))
           (iedit-show/hide-context-lines))) ;  use of parameter stops toggle off
        ((boundp 'isearch-mode)
         (if isearch-mode
             (if (window-minibuffer-p)
                 (isearch-forward-exit-minibuffer)
               ;; isearch-cancel
               ;; isearch-exit leaves mini-buffer?
               (isearch-abort))
           (if (use-region-p)
               (progn
                 (setq isearch-string
                  (regexp-quote (buffer-substring-no-properties
                                 (region-beginning) (region-end))))
                 (deactivate-mark)
                 (isearch-update))
             (isearch-forward-regexp))))
        (t (message "isearch-mode and iedit-mode undefined"))))

(defun nim-find-multiple-toggle () ; C-S-f
  "Auto-start multi-location editing or toggle inclusion of current match.
Selected region auto-starts an multi-location query.
Multio-location prefers iedit-mode, otherwise multiple-cursors-mode.
Note: multiple-cursors-mode testing failed."
  (interactive)
  (cond
   ((boundp 'iedit-mode)
    (isearch-done)
    (iedit-mode))
   ;; could not get multiple-cursors-mode to work
   ((boundp 'multiple-cursors-mode)
    (if multiple-cursors-mode
        (progn (multiple-cursors-mode 0)
               (deactivate-mark))
      (if (use-region-p)
          (mc/mark-all-like-this)
        (mc/mark-all-symbols-like-this)
        (mc/execute-command-for-all-cursors 'deactivate-mark)
        ))) ; the selection made by mc/mark-all-symbols-like-this creates problems
   (t (message "nim-find-multiple-toggle did not find iedit or multiple-cursors"))))

(defun nim-find-repeat ( N ) ; C-g
  "Goto next Nth next iedit or isearch match.
Selected region starts isearch query."
  ;; Note: Selected region is not the same as found region is not the same as...
  (interactive)
  (cond
   ((use-region-p)
    (and (boundp 'iedit-mode) iedit-mode (iedit-mode)) ; turn iedit off
    (setq isearch-string
          (regexp-quote (buffer-substring-no-properties
                         (region-beginning) (region-end))))
    (deactivate-mark)
    (isearch-repeat-forward N))
   ((and (boundp 'iedit-mode) iedit-mode)
    (iedit-next-occurrence N))
   (t
    (message "nim-find-repeat %d" N)
    (isearch-repeat-forward N))))

(defun nim-join-line()
  "Join the next line onto the end of the current line and goto the junction."
  (interactive)
  (end-of-line)
  (let ((zPoint (point)))
    (next-line)
    (delete-indentation)
    (goto-char zPoint)))

(defun nim-join-paragraph ()
  "Join all lines not separated by a blank line."
  ;; Future: could separate a single line separated by blank lines.
  ;; Future: could consider common line leader to join multi-line comments into one.
  (interactive)
  (let ((zFirstIsBlank (= (line-beginning-position) (line-end-position))))
    (next-line)
    (while (not (= (line-beginning-position) (line-end-position)))
      (if zFirstIsBlank
          (setq zFirstIsBlank nil)
        (delete-indentation))
      (next-line))))

(defun nim-kill-all-mortal ()
  "Kill all mortal buffers."
  (interactive)
  (let ((zBuff (current-buffer))
        zKilled
        zName)
    (dolist (zOther (buffer-list))
      (and (not (eq zBuff zOther))
           ;; keep the buffer-name to create a killed list for the message
           (setq zName (buffer-name zOther))
           (nim-buffer-mortalp zOther)
           (kill-buffer zOther)
           ;; zOther successfully killed
           (setq zKilled (push zName zKilled))))
    (if (car zKilled)
        (message "Killed %d mortal non-file buffers %s."
                 (length zKilled) (format "%s " zKilled))
      (message "No mortal non-file buffers remain."))))

(defun nim-kill-other-windows ()
  "`delete-window' other windows in current frame.
Except `selected-window' and `minibufferp'.
Also `kill-buffer' when `buffer-is-mortal'."
  ;; did not consider using keyboard-escape-quit
  (interactive)
  (if (minibufferp)
      (message "Cannot close all windows save the minibuffer")
    (let (zDid
          (zCurrent (selected-window)))
      (walk-windows
       (lambda (zWalk)
         (when (window-live-p zWalk)
           (unless (eq zWalk zCurrent)
             (setq zDid t)
             (let ((zBuff (window-buffer zWalk)))
               (delete-window zWalk)
               (when (nim-buffer-mortalp zBuff)
                 (kill-buffer zBuff)))))))
      (unless zDid
        (message "No other windows in this frame.")))))

(defun nim-kill-region ( PREFIX &optional COPYP APPENDP )
  "`kill-region' or function `kill-whole-line'.
PREFIX specifies number of lines to kill (negative kills backward).
COPYP also just copies.
APPENDP appends to cliboard content.
Kills forward from last kill point automatically append."
  (interactive "P")
  (when APPENDP (append-next-kill))
  (if COPYP
      ;; just copy to pasteboard
      (progn
        (unless (use-region-p) (nim-select-line))
        (copy-region-as-kill (region-beginning) (region-end)))
    ;; copy to pasteboard and delete
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-whole-line PREFIX))))

(defun nim-kill-window () ; C-q
  "`exit-minibuffer' when `minibufferp'.
else `kill-buffer' when `nim-buffer-mortalp'.
else `delete-window' (view) if multi-window.
else `delete-frame' (window) if multi-frame.
else `kill-buffer' when last window and mortal (known `delete-window').
else `nim-kill-all-mortal' if scratch and mortals.
else `kill-buffer'."
  ;; C-x C-c to exist is complex and difficult to remember.
  ;; C-z to suspend of console has undo overload, might be able to assign to M-z.
  (interactive)
  (cond ((nim-buffer-minip)
         (exit-minibuffer))
        ((nim-buffer-mortalp)
         (kill-buffer))
        ((cdr (window-list))
         (delete-window)
         (when (nim-buffer-mortalp)
           (kill-buffer)))
        ((cdr (frame-list))
         (delete-frame))
        ((eq (get-scratch-buffer-create) (current-buffer))
         ;; ((string-equal "*scratch*" (buffer-name zBuff))
         (if (seq-filter 'nim-buffer-mortalp (buffer-list))
             (nim-kill-all-mortal)
           (save-buffers-kill-emacs)))
        (t (kill-buffer))))

(defun nim-kill-word ( PREFIX )
  "Backspace until previous word.
PREFIX (interactive 'P') specifies word count (default 1)."
  (interactive "P")
  (backward-kill-word (if PREFIX PREFIX 1)))

(defun nim-next ( N )
  "Allow shift selection modification after `forward-sexp' N if without error.
Otherwise `forward-char' N."
  (interactive "^") ; the ^ is a joke because the caller must use it
  (condition-case nil
      (forward-sexp N)
    (error (forward-char N))))

(defun nim-save()
  "`nim-save-region' given selection.
Otherwise `save-buffer' after `untabify' and `delete-trailing-whitespace'."
  (interactive)
  (if (use-region-p) (nim-save-region)
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (save-buffer)))

(defun nim-save-region ()
  "Append region (or entire buffer) to another file."
  (interactive)
  (let ((zFileName (read-file-name
                    (concat "Append " (if (use-region-p) "selected region" "buffer") " to:"))))
    (let ((the-min (if (use-region-p) (min (point) (mark)) (point-min)))
          (the-max (if (use-region-p) (max (point) (mark)) (point-max))))
      (when zFileName
        ;; perform append
        (write-region the-min the-max zFileName 'APPEND)
        ;; open file and select appended region
        (find-file zFileName)
        (push-mark (point-max))
        (goto-char (- (point-max) (- the-min the-max)))
        (setq mark-active t)))))

(defun nim-scroll-horizontal-center ()
  "Scroll the window to horizontally center the point."
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

(defun nim-select-line()
  "Cause whole line(s) to be selected."
  (interactive)
  (when (> (point) (mark)) (exchange-point-and-mark))
  (beginning-of-line) ; enclude entire line at start of region
  (exchange-point-and-mark)
  (unless (bolp)
    (forward-line)
    (beginning-of-line))) ; do not extend region if end of region is at bol

(defun nim-toggle-case ()
  "Rotate CAPS, Capitalized, lower.
!!! Does not properly message !!!"
  (interactive)
  (let (zStart
        zEnd ; applied range
        zString zCase ; original values
        (deactivate-mark nil)) ; why is function deactivate-mark assigned to nil ?
      (if (use-region-p)
            (setq zStart (region-beginning) zEnd (region-end))
         (save-excursion
           (skip-chars-backward "[:alnum:]-_")
            (setq zStart (point))
            (skip-chars-forward "[:alnum:]-_")
            (setq zEnd (point))))
      (when (not (eq last-command this-command))
        (put this-command 'pState 'downcase))
      (setq zString (buffer-substring-no-properties zStart zEnd))
      (setq zCase (get this-command 'pState))
      (defun nim-toggle-case--downcase ()
        (downcase-region zStart zEnd)
        (if (string-equal zString (buffer-substring-no-properties zStart zEnd))
            (when (equal 'upcase (get 'nim-toggle-case 'pState))
              (message "No alphabetic present for case change.")))
        (put 'nim-toggle-case 'pState 'downcase))
      (defun nim-toggle-case--propercase ()
        (downcase-region zStart zEnd)
        (upcase-initials-region zStart zEnd)
        (if (string-equal zString (buffer-substring-no-properties zStart zEnd))
            (nim-toggle-case--downcase)
          (put 'nim-toggle-case 'pState 'propercase)))
      (defun nim-toggle-case--upcase ()
        (upcase-region zStart zEnd)
        (if (string-equal zString (buffer-substring-no-properties zStart zEnd))
            (nim-toggle-case--propercase)
          (put 'nim-toggle-case 'pState 'upcase)))
      (cond ((equal zCase 'upcase) (nim-toggle-case--propercase))
            ((equal zCase 'propercase) (nim-toggle-case--downcase))
            (t (nim-toggle-case--upcase)))))

(defun nim-toggle-desktop ()
  "Toggle between .emacs.desktop in $PWD and $HOME/ or $HOME/.emacs.d/."
  (interactive)
  (let ((the-dir (file-name-as-directory desktop-dirname))
        (the-pwd (file-name-as-directory (expand-file-name (getenv "PWD"))))
        (the-home (file-name-as-directory (expand-file-name (getenv "HOME")))))
    (if (equal the-dir the-home)
        (if (equal the-pwd the-home)
            (desktop-change-dir (concat the-home ".emacs.d/"))
          (desktop-change-dir the-pwd))
      (desktop-change-dir the-home))))

(defun nim-toggle-font-monospace ()
  "Toggle buffer font between Monospace and normal defaut.
Assumes 'buffer-face-mode' indicates font (obviously stupid)."
  (interactive)
  (if (and (boundp 'buffer-face-mode) buffer-face-mode)
      (buffer-face-mode 0)
    (buffer-face-set :family "Monospace")))

(defun nim-tab ( N )
  "'minibuffer-complete' when 'minibufferp'.
'forward-button' when 'buffer-read-only' (N=button count/direction).
'indent-for-tab-command' when N>0.
'indent-according-to-mode' with N<0."
  (interactive)
  (cond ((minibufferp) (minibuffer-complete))
        (buffer-read-only (forward-button N t t))
        ((>= N 0) (indent-for-tab-command))
        (t (indent-according-to-mode))))

(defun nim-toggle-hideshow ( POINT )
  "Toggle fold of text indented at or beyond cursor.
POINT = current point from interactive d."
  (interactive "^d") ; should investigate ^ more
  (back-to-indentation)
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (1+ (current-column))))
  (goto-char POINT))

(defun nim-toggle-hideshow~ ()
  "Toggle hideshow mode."
  ;; --- this does not work ---
  ;; basic commands are (hs-hide-block) (hs-show-block)
  (interactive)
  (hs-minor-mode 1)
  (if (hs-find-block-beginning)
      (hs-toggle-hiding)
      (while (and
              (not (hs-overlay-at (region-min)))
              (setq from (next-overlay-change (region-min)))
              (not (= (region-max) (region-min))))) ; locate first hs-overlay
      (if (= (region-max) (region-min)) ; hs-overlay-was-not-found
          (hs-hide-all)
        (hs-show-all))))

(defun nim-toggle-narrow () ; C-]
  "Toggle narrowing of current function or selected region.
Create/remove indirect buffers as needed."
  (interactive)
  (if (buffer-narrowed-p)
      ;; widen and kill narrowed buffer if it is indirect
      (let ((zBuff (current-buffer))
            (the-base (buffer-base-buffer)))
        (if (not the-base) (widen)
          (set-window-buffer nil the-base)
          (unless (get-buffer-window zBuff)
            (kill-buffer zBuff)))) ;; C-x n w
    ;; clone the wide buffer before narrow
    (switch-to-buffer (clone-indirect-buffer nil nil))
    (if (not (use-region-p))
        (narrow-to-defun) ;; C-x n d
      (narrow-to-region (region-beginning) (region-end)) ;; C-x n n
      )))

(defun nim-toggle-preview ()
  "Assumes LaTex preview buffer xor selected region.
Does NOT toggle anything, but nice to close the preview."
  (interactive)
  (cond
    ((eq major-mode 'latex-mode)
     (if (use-region-p)
         (tex-region (region-beginning) (region-end))
         (tex-buffer))
     (run-with-idle-timer 0.3 nil 'tex-view))
    (t (message (format
                 "nim-toggle-preview does not know how to preview %s"
                 (major-mode))))))

(defun nim-toggle-readonly () ; C-r
  "Toggle read-only mode using sudo when needed."
  (interactive)
  (if (and
       (buffer-file-name)
       (not (file-writable-p (buffer-file-name))))
      (find-alternative-file-with-sudo) ;; then ;; root privileges required
    (read-only-mode (if buffer-read-only -1 1))))

(defun nim-toggle-wrap () ; C-t
  "Toggle line wrap/truncate.
Coordinate 'toggle-truncate-lines' with 'visual-line-mode'."
  (interactive)
  (if truncate-lines
      (progn
        (toggle-truncate-lines 0)
        (visual-line-mode 1))
    (toggle-truncate-lines 1)
    (visual-line-mode 0)))

;; Turn off the minor mode in the minibuffer
;; (add-hook 'minibuffer-setup-hook nim-mode)

;; without [tab] declaration, tab will be converted to C-i
;;
;; "[C-.]" [(control .)] -- I have success with this syntax
;; "[^.]"  [(control .)]
;; "[A-.]" [(alt .)] -- No longer on keyboards.
;; "[S-.]" [(shift .)]
;; "[s-.]" [(super .)] -- I reserve this for the window manager.
;; "[H-.]" [(hyper .)] -- No longer of keyboards.
;; "[M-.]" [(meta .)] -- Meta is called Alt on keyboard
;; "[m-#]" mouse key number -- The mouse is on the floor again.
;;
;; "[C-S-.]" specifies both control and shift plus the lower case letter
;; "[M-.]" specifies meta and the case of the letter determines the inclusion of shift
;; Note: Mixing the above two syntaxes in any way does not work.
;;
;; kbd
;;   <C-down> = C-<down> = ^<down>
;;
;; C-[ is ESC as defined by terminals -- makes a mess if you redefine it even in X

;; prevent need to backspace twice when trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(bind-keys :map nim-mode-map
           ;; minor mode, nim-mode, is used to override all other minor mode keymaps !!!
           ([escape] . (lambda ()
                         "Logically identical to sending C-g to the next map.
Unfortunately, primitive C-g is does NOT cancel any better than this."
                         (interactive)
                         (cond
                          ((>= (recursion-depth) 1)
                           (abort-recursive-edit))
                          ((window-minibuffer-p)
                           (minibuffer-keyboard-quit)) ;; minibuffer*map uses 27 (not escape)
                          (t (keyboard-quit)))))
           ;; ([backspace] . backward-delete-char-untabify)
           ([delete] . nim-delete) ; Use backspace for simple delete into register 0.
           ([S-delete] . (lambda () (interactive) (nim-delete 'APPENDP)))
           ([M-up] . (lambda ()
                       "Scroll-other-window up if one exists, otherwise scroll-up this window."
                       (interactive)
                       (if (< 1 (count-windows)) (scroll-other-window -1) (scroll-up -1))))
           ([M-down] . (lambda ()
                         "Scroll-other-window down if one exists, otherwise scroll down this window."
                         (interactive)
                         (if (< 1 (count-windows)) (scroll-other-window 1) (scroll-up 1))))
           ;; foward|backward-sexpression does NOT work properly if beginning inside quotes.
           ;;   need to be able to move outside of a quoted string.
           ;; foward|backward-sexpression will error only if immediately before|after end|beginning of sexpression.
           ;;   the error can be used to flag when we are actually there.
           ;; @@@ Repair examining the font for inside quotes and move until the font changes. I can't remember the name of the commands, but those exist. @@@
           ([M-left] . (lambda () (interactive "^") (nim-next -1)))
           ([M-S-left] . nil) ;; handled by M-left via (interactive "^")
           ([M-right] . (lambda () (interactive "^") (nim-next 1)))
           ([M-S-right] . nil) ;; handled by M-right via (interactive "^")
           ;; this concept of saving and restoring the point is messy and unnecessary.
           ;; a package is installed which records the point logically and all I need is a pop-point (whatever that is).
           ("C-SPC" . point-to-register) ; store point vs C-x r SPC a-z
           ("C-S-SPC" . jump-to-register) ; recall point vs C-x r j a-z
           ("C-=" . text-scale-increase) ; vs C-x + modal (lazy on the shift key like others)
           ("C-+" . describe-text-properties) ; vs describe-face C-u C-x =
           ("C--" . text-scale-decrease) ; vs C-x - modal
           ;; ("M-=" . count-words-region) ; vs M-=
           ;; the multi-cursor mode stuff is still under review
           ("C->" . mc/mark-next-like-this) ;; when text is selected
           ("C-<" . 'mc/mark-previous-like-this) ;; when text is selected
           ("C-]" . nim-toggle-narrow) ;; wish this was C-[ because it is easier to type and makes a good visual. ;; C-x n n
           ;; ([return] . newline) ;; needed to distinguish from C-m  ;;;; overloads way no much
           ([C-return] . cua-set-rectangle-mark)
           ([C-S-return] . flyspell-correct-word-before-point)
           ([C-next] . (lambda ()
                         "Like 'next-buffer', but remember buffer order."
                         (interactive)
                         (switch-to-buffer (nim-buffer 1)))) ; vs scroll-left
           ([C-prior] . (lambda ()
                          "Like 'previous-buffer', but remember buffer order."
                          (interactive)
                          (switch-to-buffer (nim-buffer -1)))) ; vs scroll-right
           ([C-S-next] . (lambda ()
                           "Move buffer foward 1 position in list used by [C-next] and [C-prior]."
                           (interactive)
                           (nim-buffer-move 1))) ; vs scroll-left ([C-S-prior] . (lambda ()
           ([C-S-prior] . (lambda ()
                            "Move buffer backward 1 position in list used by [C-next] and [C-prior]."
                            (interactive)
                            (nim-buffer-move -1))) ; vs scroll-right
           ("C-1" . nim-kill-other-windows ) ; vs C-x 1 ; was digit-argument
           ("C-2" . split-window-below) ; vs C-x 2 ; was digit-argument
           ("C-3" . split-window-right) ;  vs C-x 3 ;  was digit-argument
           ("C-6" . enlarge-window) ; vs C-x C-^ ; was digit-argument
           ("C-^" . shrink-window) ; was digit-argument
           ("C-7" . enlarge-window-horizontally) ; vs C-x {
           ("C-&" . shrink-window-horizontally) ; vs C-x }
           ("C-a" . nim-bol) ; vs beginning-of-visual-line
           ("C-b" . list-buffers ) ; vs C-x C-b
           ("C-S-b" . nim-binary-toggle)
           ("C-c" . (lambda ( PREFIX ) (interactive "P")
                      (nim-kill-region PREFIX 'COPYP))) ; was kill-visual-line (until eol)
           ("C-S-c" . (lambda ( PREFIX ) (interactive "P")
                        (nim-kill-region PREFIX 'COPYP 'APPENDP)))
           ("C-d" . (lambda () (interactive) (nim-find-repeat 1))) ; was delete-char
           ("C-S-d" . (lambda () (interactive) (nim-find-repeat -1)))
           ("M-d" . nim-toggle-desktop)
           ("C-e" . nim-eol) ; end-of-visual-line
           ("M-e" . nim-eval) ; vs C-x C-e (and eval-buffer)
           ("C-f" . nim-find)
           ("C-S-f" . nim-find-multiple-toggle)
           ("M-f" . (lambda () (interactive) (what-cursor-position t))) ; t prefix argument
           ("C-g" . (lambda () (interactive) (nim-find-repeat 1))) ; prefer C-y for paste
           ("C-S-g" . (lambda () (interactive) (nim-find-repeat -1)))
           ;; ("M-g g . goto-line) ; interactively obtains the prefix argument
           ;; ("C-h b . describe-bindings) ; show current buffer keyboard bindings
           ;; ("C-h f . describe-function) ; read function name and describe it
           ;; ("C-h k . describe-key) ; read key sequence and describe it
           ;; ("C-h v . describe-variable) ; read variable name and describe it
           ("C-S-h" . nim-toggle-hideshow)
           ([tab] . (lambda () (interactive) (nim-tab 1))) ; without <tab> declaration, emacs converts tab to C-i
           ([S-tab] . (lambda () (interactive) (nim-tab -1)))
           ("C-i" . (lambda () (interactive)
                      "Insert a line before current line."
                      (forward-line '-1) (end-of-line) (newline-and-indent))) ; vs kill-visual-line (until eol)
           ("C-S-i" . (lambda () (interactive)
                        "Insert a line after current line."
                        (end-of-line) (newline-and-indent)))
           ("M-i" . edebug-defun)
           ("C-j" . nim-join-line)
           ("C-l" . goto-line)
           ("C-S-j" . nim-join-paragraph)
           ("C-k" . (lambda ( PREFIX ) (interactive "P")
                      (nim-kill-region PREFIX))) ; vs kill-visual-line (until eol)
           ("C-S-k" . (lambda ( PREFIX ) (interactive "P")
                        (nim-kill-region PREFIX nil 'APPENDP)))
           ;; C-l . recenter-top-bottom
           ("C-S-l" . nim-scroll-horizontal-center )
           ;; "c-m" must overload [return] to unbind for this overload
           ("C-S-m" . nim-toggle-font-monospace)
           ("C-n" . (lambda () (interactive) (nim-error-next 1))) ; vs next-line
           ("C-S-n" . (lambda () "Create a new empty buffer." (interactive)
                        (switch-to-buffer
                         (generate-new-buffer
                          (generate-new-buffer-name "*Unsaved*")))))
           ("C-o" . other-window) ; other vs C-x o
           ("C-S-o" . find-file-at-point)
           ("M-S-o" . auto-revert-mode)
           ("C-p" . (lambda () (interactive) (nim-error-next -1)))
           ("C-S-p" . nim-toggle-preview) ; vs foward-line
           ("C-q" . nim-kill-window) ; was quoted-insert (@also C-w)
           ("C-r" . nim-toggle-readonly) ; replacing isearch-backward
           ("C-S-r" . insert-file)
           ("C-S-q" . quoted-insert) ; was C-q
           ("C-s" . nim-save) ; vs C-x C-s
           ("C-S-s" . nim-save-region) ; to reason for this becaase nim-save will do it.
           ("C-t" . nim-toggle-wrap) ; C-x x t
           ("C-S-t" . nim-toggle-case) ; was transpose-chars
           ;; ("C-u" . universal-argument)
           ("C-v" . yank) ; ; was scroll-up-command
           ("C-S-v" . yank-pop) ; was not defined (defaulted to C-v)
           ("C-w" . nim-kill-word) ; was kill-region
           ("C-x e" . execute-extended-command) ; avoid M-x
           ;; ("C-x C-e" . eval-last-sexp) ; point at closing parentheses
           ("C-x C-f" . find-file-at-point) ; was find-file
           ("C-y" . yank) ; ; was scroll-up-command
           ("C-S-y" . yank-pop) ; was scroll-up-command
           ("C-z" . undo-fu-only-undo) ; undo vs C-_
           ("C-S-z" . undo-fu-only-redo) ; redo vs C-g C-_ where C-g is a toggle ;  may (keyboard-quit) [control-g] to redo after logical top
           )
