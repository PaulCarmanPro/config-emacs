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
  "Default test function for nim-ring-next-match")

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

(defun nim-buffer-is-mortal (BUFF)
  "Should BUFF be killed after closing its window?"
  (let ((zName (buffer-name BUFF)))
	 (not (or (buffer-file-name BUFF) ;; not associated with a file
			    (get-buffer-window BUFF) ;; not currently displayed in a window
			    (string-equal "*scratch*" zName) ;; not the scratch buffer
			    (string-match "^untitled(<[0-9]+>)?$" zName) ;; not a new untitled buffer awaiting save
			    (string-match "^ *Minibuf-[0-9]+*)?$" zName))))) ;; not a minibuffer

;;;;;;;;;;;;;;;;;; -- used to order buffers -- filter
;; buffer-type - used to determine buffers that are regularly visited vs not
;; enforced by keystroke declarations
;;;;;;;;;;;;;;;;

(defcustom nim-buffer-names-unwanted '("*Flycheck error messages*" ; internal error - cannot become current
                                       "*Messages*" ; perpetual
                                       )
  "Buffer names understood by 'nim-buffer-wanted' as being unwanted.
The default may be unwanted, but this is for assurance if default changes."
  :group 'string)

(defcustom nim-buffer-names-wanted '("*Packages*" "*scratch*") ; list-packages
  "Buffer names understood by 'nim-buffer-wanted' as being unwanted."
  :group 'string)

(defun nim-buffer-is-wanted (&optional BUFF)
  "Should buffer BUFF be included by 'nim-buffer-ringset'?"
  (let ((zName (buffer-name BUFF)))
    (cond ((not zName) ; killed buffer
           nil)
          ((or (buffer-file-name BUFF) ; associated with a file
               (equal "dired-mode" (with-current-buffer BUFF major-mode))
               (member zName nim-buffer-names-wanted))
           t)
          ((or (string-equal " " (substring zName 0 1))
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
Use 'nim-buffer-ringset' for a updated buffer ringset.")
(setq _nim-buffer-ringset nil) ;; rebuild malformed ringset

(defun nim-buffer-ringset (&optional BUFF)
  "Verify wanted buffers in ordered ringset.
The car value of the result will be BUFF or 'current-buffer'.
Filters unwanted buffers and inserts wanted buffers.
Return value is stored in '_nim-buffer-ringset'."
  (let ((zRing (nim-ring-with _nim-buffer-ringset #'nim-buffer-is-wanted)))
    ;; insert wanted
    (dolist (the-buff (buffer-list))
      (when (nim-buffer-is-wanted the-buff)
        (setq zRing (nim-ringset-with-car the-buff zRing))))
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
  "Shift BUFF in 'nim-buffer-ringset' DELTA relative to its current position.
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

(defun nim-find () ; C-f
  "Begin 'isearch-mode' with selected region otherwise...
iedit-mode and caret hovers selection then toggle selection inclusion.
iedit-mode then toggle visibility of non-matching lines.
otherwise start normal isearch query."
  (interactive)
  ;; i-edit font-lock issue causes collusion between i-edit region and emacs region.
  ;; i-edit mode needs turned off and on to eliminate selection collusion issues.
  (if (and (boundp 'iedit-mode) iedit-mode)
      (condition-case nil ; catches if outside selection.
			 (let ((the-return (iedit-toggle-selection))) ; vs M-;
				message (concat "Trapped: " the-return))
		  (error (iedit-show/hide-context-lines 2)))
    (if (not (use-region-p))
        (isearch-forward-regexp)
	   (setq isearch-string
            (regexp-quote (buffer-substring-no-properties
                           (region-beginning) (region-end))))
	   (deactivate-mark)
      (isearch-update))))

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
   (t (message "Did not find iedit or multiple-cursors"))))

(defun nim-find-repeat ( iPrev ) ; C-g
  "Goto next match unless IPREV then goto previous match.
Selected region auto-starts an normal isearch query."
  ;; Note: Selected region is not the same as found region is not the same as...
  (interactive)
  (cond ; execute first successful thens in list of if thens
   ((use-region-p)
    (and (boundp 'iedit-mode) iedit-mode (iedit-mode)) ; turn iedit off
	 (setq isearch-string
          (regexp-quote (buffer-substring-no-properties
                         (region-beginning) (region-end))))
	 (deactivate-mark)
	 (if iPrevHv
        (face-at-point nil t)
        (isearch-repeat-backward)
      (isearch-repeat-forward)))
	((and (boundp 'iedit-mode) iedit-mode)
	 (if iPrev (iedit-next-occurrence -1) (iedit-next-occurrence 1)))
	((and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (message "nim-find-repeat %s" (if iPrev "previous" "next"))
    (if iPrev (mc/cycle-backward) (mc/cycle-forward)))
   (t (if iPrev (isearch-repeat-backward) (isearch-repeat-forward)))))

(defun nim-goto-end-of-line ()
  "Goto logical enl, then physical eol, then next logical eol."
  (interactive "^")
  (if truncate-lines
      (end-of-visible-line)
    (let ((the-point (point)))
      (end-of-visual-line)
      (when (equal the-point (point))
        (end-of-visible-line)
        (when (equal the-point (point))
          (end-of-visual-line 2))))))

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

(defun nim-kill-lines ()
  "Kill whole lines."
  (interactive)
  (if (not (use-region-p))
      (kill-whole-line)
	 (when (> (point) (mark)) (exchange-point-and-mark))
	 (beginning-of-line) ; enclude entire line at start of region
	 (exchange-point-and-mark)
	 (unless (bolp)
		(forward-line)
		(beginning-of-line)) ; do not extend region if end of region is at bol
	 (kill-region (mark) (point))))

(defun nim-kill-buffer () ; C-w
  "Kill mortal buffer or switch to scratch (leaving window open)."
  ;; code does not provide easy (switch-to-buffer "*scratch*")
  ;; !!! need to kill window if buffer is in another window !!!
  (interactive)
  (let ((the-buff (current-buffer)))
    (cond ((string-equal "*scratch*" (buffer-name the-buff))
           (nim-kill-all-mortal))
          (t (kill-buffer the-buff)))))

(defun nim-kill-all-mortal ()
  "Kill all mortal buffers."
  (interactive)
  (let ((the-buff (current-buffer))
        zKilled
        zName)
	 (dolist (zOther (buffer-list))
		(and (not (eq the-buff zOther))
			  ;; keep the buffer-name to create a killed list for the message
			  (setq zName (buffer-name zOther))
			  (nim-buffer-is-mortal zOther)
			  (kill-buffer zOther)
			  ;; zOther successfully killed
			  (setq zKilled (push zName zKilled))))
	 (if (car zKilled)
		  (message "Killed %d mortal non-file buffers %s."
                 (length zKilled) (format "%s " zKilled))
		(message "No mortal non-file buffers remain."))))

(defun nim-kill-other-windows ()
  "'delete-window' all windows except 'selected-window'.
Use 'kill-buffer' when 'buffer-is-mortal'."
  (interactive)
  (let ((zWind (selected-window)))
	 (dolist (zOther (window-list))
		(when (not (eq zOther zWind))
		  (let ((the-buff (window-buffer zOther)))
			 (delete-window zOther)
			 (when (nim-buffer-is-mortal the-buff)
				(kill-buffer the-buff)))))))

(defun nim-kill-window ()
  "Close the current window.
If last window, hten 'save-buffers-kill-emacs'.
Otherwise 'delete-window' or 'delete-frame'.
Use 'kill-buffer' when 'buffer-is-mortal'."
  (interactive)
  (let ((the-buff (current-buffer)))
	 (if (not (or (cdr (window-list nil "no-minibuf"))
					  (cdr (frame-list))))
		  (save-buffers-kill-emacs) ;; is last window of last frame
		(if (and
			  (cdr (frame-list))
			  (not (cdr (window-list nil "no-minibuf"))))
			 (delete-frame) ;; is last winodw with more frames
		  (delete-window)) ;; more windows in the frame
		(when (nim-buffer-is-mortal the-buff) (kill-buffer the-buff)))))

(defun nim-toggle-case ()
	"Rotate CAPS, Capitalized, lower."
	(interactive)
	(let (zStart
         the-end
         zOriginal
         zState
         (deactivate-mark nil))
		(if (use-region-p)
				(setq zStart (region-beginning) the-end (region-end))
			(save-excursion
			  (skip-chars-backward "[:alnum:]-_")
				(setq zStart (point))
				(skip-chars-forward "[:alnum:]-_")
				(setq the-end (point))))
		(when (not (eq last-command this-command))
		  (put this-command 'state 0))
      (setq zOriginal (buffer-substring-no-properties zStart the-end))
      (setq zState (get this-command 'state))
      (defun nim-toggle-case--do-state-0 ()
        (upcase-region zStart the-end)
	     (put this-command 'state 1)
        (when (string-equal zOriginal (buffer-substring-no-properties zStart the-end))
          (nim-toggle-case--do-state-1))
        (when (equal 0 (get this-command 'state))
          (message "No alphabetic present for case change.")))
      (defun nim-toggle-case--do-state-1 ()
        (downcase-region zStart the-end)
	     (upcase-initials-region zStart the-end)
	     (put 'nim-toggle-case 'state 2)
        (when (string-equal zOriginal (buffer-substring-no-properties zStart the-end))
          (nim-toggle-case--do-state-2)))
      (defun nim-toggle-case--do-state-2 ()
        (downcase-region zStart the-end)
	     (put this-command 'state 0))
		(cond ((equal 0 zState) (nim-toggle-case--do-state-0))
		      ((equal 1 zState) (nim-toggle-case--do-state-1))
				((equal 2 zState) (nim-toggle-case--do-state-2)))))

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

(defun nim-toggle-narrow () ; C-]
  "Toggle narrowing of current function or selected region.
Create/remove indirect buffers as needed."
  (interactive)
  (if (buffer-narrowed-p)
		;; widen and kill narrowed buffer if it is indirect
		(let ((the-buff (current-buffer))
            (the-base (buffer-base-buffer)))
		  (if (not the-base) (widen)
			 (set-window-buffer nil the-base)
			 (unless (get-buffer-window the-buff)
				(kill-buffer the-buff)))) ;; C-x n w
	 ;; clone the wide buffer before narrow
	 (switch-to-buffer (clone-indirect-buffer nil nil))
	 (if (not (use-region-p))
		  (narrow-to-defun) ;; C-x n d
		(narrow-to-region (region-beginning) (region-end)) ;; C-x n n
      )))
  
(defun nim-toggle-readonly () ; C-r
  """Toggle read-only mode using sudo when needed."""
  (interactive)
  (if (and
		 (buffer-file-name)
		 (not (file-writable-p (buffer-file-name))))
		(find-alternative-file-with-sudo) ;; then ;; root privileges required
    (read-only-mode (if buffer-read-only -1 1))))

(defun nim-toggle-wrap () ; C-t
  "Toggle line wrap/truncate.
Cooridnate with 'visual-line-mode' (word-wrap)."
  (interactive)
  (if truncate-lines
      (progn
        (toggle-truncate-lines 0)
        (visual-line-mode 1))
    (toggle-truncate-lines 1)
    (visual-line-mode 0)))
    
;;;;;;;;;;;;;;;;
;; get unwanted key declarations out of the way
;;;;;;;;;;;;;;;;

(defun nim-remove-key (keymap key)
  "Remove KEY from KEYMAP.
Might be able to master Ctrl-[ which loves to come back with ESC."
	;; pulled this off of the web and it is beyond my understanding
   (define-key keymap key nil)
   (setq key (cl-mapcan (lambda (k)
                          (if (and (integerp k)
                                   (/= (logand k ?\M-\^@) 0))
                              (list ?\e (- k ?\M-\^@))
                            (list k)))
                        key))
   (if (= (length key) 1)
       (delete key keymap)
     (let* ((prefix (vconcat (butlast key)))
            (submap (lookup-key keymap prefix)))
       (delete (last key) submap)
       (when (= (length submap) 1)
         (nim-remove-key keymap prefix)))))

;; Attempt to rid '[C-return]' and cua-'C-x' bindings.
;; My region deletes fill paste buffer = 'C-x' has to editing purpose.
;; Overloading 'C-x' and 'C-c' creates unwanted issues.
(if (boundp 'cua-global-keymap)
    (nim-remove-key cua-global-keymap (kbd "<C-return>")))
(if (boundp 'cua--prefix-repeat-keymap)
    (nim-remove-key cua--prefix-repeat-keymap (kbd "C-x")))

;;;;;;;;;;;;;;;;
;; declare minor mode to give precedence to key declarations
;;;;;;;;;;;;;;;;

;; Main use is to have my key bindings have the highest priority
(defvar nim-mode-map (make-sparse-keymap) "Keymap for nim-mode.")

(define-minor-mode nim-mode
  "A minor mode to override major mode keybindings."
  :init-value t ;; needed to get enabled in `fundamental-mode' buffers even after \"(global-nim-mode 1)\".
  :lighter " nim"
  :keymap nim-mode-map)
(provide 'nim-mode) ;; add `nim-mode' to global variable `features'
(define-globalized-minor-mode global-nim-mode nim-mode (lambda()(nim-mode t))) ;; turn on in every buffer
;; `emulation-mode-map-alists' maps preside over `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((nim-mode . ,nim-mode-map)))

;; Turn off the minor mode in the minibuffer
;;(add-hook 'minibuffer-setup-hook nim-mode)

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
								 "!!! Wish I could send C-g to the next map !!!
(nim-mode 0) (call-interactively (global-key-binding \"\\C-g\")) (nim-mode 1) errors because nim-mode not found  !!!"
								 (interactive)
								 (if (window-minibuffer-p)
									  (minibuffer-keyboard-quit) ;; certainly not ideal -- minibuffer*map uses 27 (not escape)
									(keyboard-quit))))
;;                         (call-interactively (global-key-binding "\C-g"))))
			  ([delete] . (lambda ()
								 "Kill active region to the yank stack.
Otherwise, perform nomral delete.
Use backspace for an emacs delete into register 0."
								 (interactive)
								 (if (use-region-p)
									  (delete-active-region t) ;; yank the region onto the stack
									(delete-char 1)))) ;; forget singly removed characters
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
			  ([M-left] . (lambda ()
								 "Previous sexpression."
								 (interactive "^")
								 (condition-case nil (backward-sexp)
									(error (backward-char)))))
			  ([M-S-left] . nil) ;; handled by M-left via (interactive "^")
			  ([M-right] . (lambda ()
								  "Next sexpression."
								  (interactive "^")
								  (condition-case nil (forward-sexp)
									 (error (forward-char)
											  (condition-case nil
													(foward-sexp) (error nil))))))
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
			  ([C-return] . (lambda () ;; was cua-set-rectangle-mark
									"If 'use-region-p' then multi-cursor stuff is still in review.
Otherwise cua-set-rectangle-mark is useful."
									(interactive)
									(if (not (use-region-p))
										 (cua-set-rectangle-mark) ;; when no selection
									  (if (= 1 (count-lines (region-beginning) (region-end)))
											(mc/mark-all-like-this) ;; when simple selection
										 (mc/edit-lines))))) ;; when muli-line selection
           ([M-return] . flyspell-correct-word-before-point)
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
			  ("C-a" . (lambda (iPoint)
							 "Goto logical start of line, physical start of line, or select all when empty line."
							 (interactive "^d")
                      (back-to-indentation)
							 (when (eq iPoint (point))
                        (if (bolp)
                            (if (not (use-region-p))
                                (mark-whole-buffer)
                              (pop-mark)
                              (goto-char (mark))))
                        (beginning-of-visual-line)))) ; vs beginning-of-visual-line
			  ("C-b" . list-buffers ) ; vs C-x C-b
			  ("C-S-b" . nim-binary-toggle)
			  ("M-c" . kill-region); kill vs C-w
			  ("M-d" . nim-toggle-desktop) ; vs delete-char
           ("C-e" . nim-goto-end-of-line)
			  ;; ("C-e") ; end-of-visual-line
			  ;; ("C-S-e") ; extend selection to end-of-visual-line
			  ("M-e" . eval-last-sexp) ; vs C-x C-e
			  ("M-E" . (lambda ()
                      "Evaluate buffer with message"
                      (interactive)
                      (message "eval-buffer %s" (current-buffer))
                      (eval-buffer))) ; was forward-sentence
			  ("C-f" . nim-find)
			  ("C-S-f" . nim-find-multiple-toggle)
			  ("M-f" . (lambda () (interactive) (what-cursor-position t))) ; t prefix argument
			  ("C-g" . (lambda () (interactive) (nim-find-repeat nil))) ; problem-matic keystroke to overload
			  ("C-S-g" . (lambda () (interactive) (nim-find-repeat t)))
			  ("C-S-h" .
				(lambda () (interactive)
				  (hs-minor-mode 1)
				  (if (hs-find-block-beginning)
						(hs-toggle-hiding)
					 (let ((from (point-min))
							 (to (point-max)))
						(while (and
								  (not (hs-overlay-at from))
								  (setq from (next-overlay-change from))
								  (not (= to from)))) ; locate first hs-overlay
						(if (= to from) ; hs-overlay-was-not-found
							 (hs-hide-all)
						  (hs-show-all))))))
			  ([tab] . ; without <tab> declaration, tab will be converted to C-i
				(lambda () (interactive)
				  (cond ((minibufferp) (minibuffer-complete))
						  (buffer-read-only (forward-button 1 t t))
						  (t (indent-for-tab-command)))))
			  ([S-tab] . ; without <tab> declaration, tab will be converted to C-i
				(lambda () (interactive)
				  (cond (buffer-read-only (forward-button -1 t t))
						  (t (indent-according-to-mode)))))
			  ("C-i" . (lambda () (interactive)
							 "Insert a line before current line."
							 (forward-line '-1) (end-of-line) (newline-and-indent))) ; vs kill-visual-line (until eol)
			  ("C-S-i" . (lambda () (interactive)
								"Insert a line after current line."
								(end-of-line) (newline-and-indent)))
           ("M-i" . edebug-defun)
			  ("C-j" . nim-join-line)
			  ("C-S-j" . nim-join-paragraph)
			  ("C-k" . nim-kill-lines) ; vs kill-visual-line (until eol)
			  ;; C-l . recenter-top-bottom
			  ("C-S-l" . (lambda ()
								"Scroll the window to horizontally center the point."
								(interactive)
								(let ((mid (/ (window-width) 2))
										(line-len (save-excursion (end-of-line) (current-column)))
										(cur (current-column)))
								  (if (< mid cur)
										(set-window-hscroll (selected-window)
																  (- cur mid))))))
			  ;; "c-m" must overload [return] to unbind for this overload
			  ("C-S-m" . (lambda ()
								"Toggle buffer font between Monospace and normal defaut.
Assumes buffer-face-mode is indicative of this which is obviously stupid."
								(interactive)
								(if (and (boundp 'buffer-face-mode) buffer-face-mode)
									 (buffer-face-mode 0)
								  (buffer-face-set :family "Monospace"))))
			  ("C-n" . flycheck-next-error) ; vs next-line ; this needs emacs version 26
			  ("C-S-n" . (lambda ()
								"Create a new empty buffer."
								(interactive)
								(switch-to-buffer (generate-new-buffer (generate-new-buffer-name "untitled")))))
			  ("C-o" . other-window) ; other vs C-x o
			  ("C-S-o" . find-file-at-point)
			  ("C-p" . flycheck-previous-error)
			  ("C-S-p" . (lambda ()
								"Preview a latex buffer or selected region."
								(interactive)
								(if (use-region-p)
									 (tex-region (region-beginning) (region-end))
								  (tex-buffer))
								(run-with-idle-timer 0.3 nil 'tex-view))) ; vs foward-line
           ("C-r" . nim-toggle-readonly) ; replacing isearch-backward
			  ("C-S-r" . insert-file)
           ("M-r" . find-alternate-file) ; was C-x C-v (more useful than revert-buffer)
           ("C-q" . nim-kill-window) ; was quoted-insert (@also C-w)
           ("C-S-q" . quoted-insert) ; was C-q
           ("M-R" . auto-revert-mode)
			  ("C-s" . (lambda () (interactive) (basic-save-buffer))) ; vs C-x C-s
			  ("C-S-s" . (lambda ()
								"Append entire buffer or selected region to another file."
								(interactive)
								(let ((zFileName
                               (read-file-name  ; FILENAME ; into this file name
										  (concat "Write " (if (use-region-p) "selected region" "buffer") " to:"))))
								  (when zFileName
									 (progn
										(if (use-region-p)
											 (write-region (region-beginning) (region-end) zFileName) ; append region to file
										  (write-file zFileName))
										(find-file zFileName)))))) ; open the file
			  ("C-t" . nim-toggle-wrap) ; C-x x t
			  ("C-S-t" . nim-toggle-case) ; was transpose-chars
			  ("C-S-v" . cua-yank-pop) ; was cua-yank ; better if cua-paste-pop-rotate-temporarily
			  ("C-w" . nim-kill-buffer) ; was kill-region
			  ("C-v" . cua-paste) ; just paste now
           ;; "C-x" ; never overload (cua overload disconnected above)
			  ("C-x C-f" . find-file-at-point) ; was find-file
			  ;; "M-x" . execute-extended-command
			  ("M-S-x" .  eval-expression) ;; was M-: or M-S-; ;;;; I have no idea why this does not work
			  ("C-y" . (lambda ()
							 "Insert the paste buffer at beginning of line for insertion whole lines."
							 (interactive)
							 (beginning-of-line)
							 (cua-paste nil)))
			  ("C-S-y" . cua-paste-pop) ; vs M-Y
			  ("C-z" . undo-fu-only-undo) ; undo vs C-_
			  ("C-S-z" . undo-fu-only-redo) ; redo vs C-g C-_ where C-g is a toggle ;  may (keyboard-quit) [control-g] to redo after logical top
			  )
