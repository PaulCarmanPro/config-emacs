;;
;; init.el.modeline.el (included by init.el)
;;
;; modeline constructs	  		
;;    	(list ...)	  causes percent expansion of string items
;;    	(list ...)	  evaluates subexpressions creating a flat list
;;    	'(...)	  does NOT evaluate sublists (may create unwanted not flat)
;;    	'symbol	  delay symbol evaluation until read (instead of during setq)
;;    string at top level of a list	  	string items undergo percent expansion
;;    	%-	  one or more dashes (-), to fill all remaining space in the line/title
;;    	%[	  recursive editing level, one [ for each level deep
;;    	%]	  recursive editing level, one ] for each level deep
;;    	%@	  default-directory status - @ if remote, - if local
;;    	%*	  buffer status - % if read-only, otherwise * if modified, otherwise -
;;    	%&	  buffer status - * if modified, otherwise -
;;    	%%	  literal %
;;    	%+	  buffer status - * if modified, otherwise % if read-only, otherwise -
;;    	%b	  buffer name
;;    	%c	  column number, first column is zero
;;    	%C	  column number, first column is one
;;    	%e	  out-of-memory error message, if any
;;    	%f	  file name, with path
;;    	%F	  frame name
;;    	%i	  buffer size, in bytes
;;    	%I	  buffer size, auto-scaled to K/M/G
;;    	%l	  line number
;;    	%n	  Narrow if the window is narrowed, otherwise nothing
;;    	%p	  percent of buffer above top of window
;;    	%P	  percent buffer above bottom of window
;;    	%s	  process status, or no process
;;    	%t	  text/binary file mode (if platform makes distinction-Windows)
;;    	%z	  coding systems in effect for keyboard, terminal, and file/buffer
;;    	%Z	  same as %z plus end-of-line format
;;    symbol	  	
;;    	t	  ignored
;;    	nil	  ignored
;;    	otherwise	  evaluated for value whose contents are NOT searched for %
;;    	nil `risky-local-variable' property	  properties in string, as well as :eval and :propertize, are ignored
;;    car of list	  	
;;    	string	  process list as separate modeline construct
;;    	list	  process list as separate modeline construct
;;    	unsigned integer	  unsigned integer space pads resulting string to width specified
;;    	negative integer	  negative integer truncates resulting string to width specified
;;    	:eval	  cdr of list is evaulated and result is used (do not load a file)
;;    	:propertize	  2nd item is modeline while rest are text properties to append to result string
;;    	nil symbol	  process cadr (2nd item) as a modeline list (caddr is ignored)
;;    	non-nil symbol	  process caddr (3rd item) is a modeline list (cadr is ignored)
;;    built-in variables	  	
;;    	mode-line-buffer-identification	  Buffer/file name
;;    	mode-line-client	  Identifies frames created by emacsclient
;;    	mode-line-end-spaces	  A line of dashes for text terminals; nothing for graphics displays
;;    	mode-line-frame-identification	  Frame title, for some environments
;;    	mode-line-front-space	  Dash (-) for text terminals, a space ( ) for graphic displays
;;    	mode-line-misc-info	  Defun name for ‘which-function-mode’, clock for ‘display-time-mode’
;;    	mode-line-modes	  Major and minor modes, in parenthesis; square brackets if RecursiveEdit
;;    	mode-line-modified	  Modified and read-only status
;;    	mode-line-mule-info	  Information on character sets, encodings, and other human-language details
;;    	mode-line-position	  Line, column, and portion (percent) of point in buffer; total size
;;    	mode-line-remote	  At-sign (@) for buffers visiting remote files, otherwise a dash

;;; Code:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
(setq-default
 mode-line-format
 (list
  (list :propertize "%I " 'face
		  '(:inherit "line-number-current-line"
                   :foreground "yellow")) ;; human buffer size
  (list :propertize "%c " 'face
        '(:inherit "line-number-current-line")) ; zero based column number
  (list :propertize 'mode-line-modes 'face
        '(:inherit "default")) ; Major and minor modes, in parenthesis; square brackets if RecursiveEdit
  (list :propertize "%* " 'face
        '(:inherit "default"
                   :size 1.1
                   :foreground "yellow")) ; %|*|- == readonly|modified|saved
  'mode-line-buffer-identification)) ; Buffer/file name

;;;  ;; (window-body-width nil 'pixelwise) # return the width in pixels -- have no idea how accurate this is
;;;  ;; bottom line: window-body-width returns a width which is to wide -- no idea why
;;;  ;; the mode-line font doesn't seem to be proportional, but I don't remember how I set the font.
;;;  ;; @@@ HACK: subtract an extra from the width and it works when fully wide on my machine
;;;  ;; @@@ Otherwise, it still does not work.
;;;   
;;;  (defun nim-modeline-contents (a-left a-middle a-right)
;;;    "Return a string of `window-total-width' length containing LEFT, and RIGHT aligned respectively."
;;;    (let* ((the-length-left (length (format-mode-line a-left)))
;;;     			 (the-length-middle (length (format-mode-line a-middle)))
;;;     			 ;; length of " %*" does not calculate and is therefore hardcoded as 2
;;;     			 (the-length-right (length (format-mode-line a-right)))
;;;     			 (the-extra (- (window-body-width) the-length-left the-length-middle the-length-right 10))
;;;     			 (the-extra-left (/ the-extra 2))
;;;     			 (the-extra-right (- the-extra the-extra-left))
;;;     			 (the-space-left (list (make-string the-extra-left ? )))
;;;     			 (the-space-right (list (make-string the-extra-right ? ))))
;;;      (append a-left the-space-left a-middle the-space-right a-right)))
;;;
;;;  (setq-default mode-line\-format
;;;     	  	'((:eval
;;;     	  		 (nim-modeline-contents
;;;     	  			'("%e" ; full memory error message
;;;     	  				"%n" ; Narrow if appropriate?
;;;     	  				mode-line-modes ; major/minor mode indicator
;;;     	  				)
;;;     	  			'("%Z " ; terminal, buffer, eol coding mnemonics
;;;     	  				"%c:%l") ; column:line 
;;;     	  			`(vc-mode vc-mode ; version control?
;;;     	  				,(or (buffer-file-name) (buffer-name)) ; visible-name
;;;     	  				" %*" ; %|*|- == readonly|modified|saved
;;;     	  				mode-line-misc-info
;;;     	  				mode-line-end-spaces)))))
