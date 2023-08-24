;;; init.el.require.el --- included by init.el

;;; Commentary:

;; (list-packages) ; wait a long time for package list buffer to load and open
;; installed packages exist in ~/.emacs.d/elpa/ (is a list of installed packages)
;;     d = mark for deletion
;;     i = mark for install
;;     u = unmark
;;     x = execute marks
;;     r = refresh list
;;     w = open website
;;     U = mark all that need update
;;     <del> = remove mark
;; variable package-activated-list contains names of currently activated packages.
;; iteresting to view $ grep '(define-derived-mode' /7/deb-emacs/**/*.el for a mode list.

;; ~/.emacs.d/elpa/ files will be downloaded/updated when required here via the package system
;; ~/.emacs.d/lisp/ files were manually downloaded/created and need to be kept

;; built-in
(require 'package) ; the package handler
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/")) ; more packages
(package-initialize) ; auto-load packages -- do not understand why I need or want this
(add-to-list 'load-path "~/.emacs.d/elpa/") ; search for packages in this directory
(add-to-list 'load-path "~/.emacs.d/lisp/") ; search for packages in this directory
(add-to-list 'load-path "~/.emacs.d/lisp/vcard-0.1/") ; load-path is not recursive

;; @see https://github.com/jwiegley/use-package
;;
;; this was created for lsp-mode
;; @see https://anarc.at/blog/2022-04-27-lsp-in-debian/
;; it brings language help into emacs, but is quite complex
;;
;;;; (eval-when-compile
;;;;   (if (require 'use-package)
;;;;       (progn (setq use-package-hook-name-suffix nil) ; do not add -hook to hook varible
;;;;              (setq use-package-always-ensure t)) ; assume :ensure t for every use-package
;;;;     (message "!!! Serious problem: Could not require use-package !!!")))
;;
;; :ensure t does not cause automatic package updates
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))
;;
;; bind = literally (bind-key "key" 'command)
;; hook = (add-hook 'name-hook . #'command)
;;
;; use-package
;;   commands which imply :defer t
;;      :bind (("key" . command) ; global key map
;;         :map package-key-map ("key" . command))
;;      :bind*
;;      :bind-keymap ("key" . package-keymap) ; autoload package due to keypress
;;      :bind-keymap*
;;      :commands (list of commands inside package to autoload when the commands are first used)
;;      :mode
;;      :interpreter
;;      :hook (mode-without-suffix-hook) ; implies this package to be activated (command is the package name)
;;      :hook (mode-without-suffix-hook . command) ; causes command to be included in :commands
;;      :magic ("magic-beginning-of-buffer-string" . command)
;;   :after (list of packages that must be loaded first)
;;   :after (:all list of packages that must be loaded first)
;;   :after (:any list of packages where one must be loaded first) ; where :all and :any may be nested
;;   :catch t ; enable error catching
;;   :catch nil ; disable error catching
;;   :catch (lambda (aKeyword aError) (message (error-message-string aError) ; handle error
;;   :commands command : introduce command for byte compiler
;;   :config (code to execute after package is loaded)
;;   :custom (variable-name custom-value "optional document string") ; custom value stored here (possible conflicts)
;;   :custom-face (variable-face (face-value))
;;   :defer t ; delay package loading until a later condition is met
;;   :defines variable ; introduce variable for byte compiler
;;   :delight ; if you have delight installed and mode matches package name
;;   :delight minor-mode-symbol ; if you have delight installed
;;   :delight '(:eval (command)) ; if you have delight installed
;;   :delight (command) ; if you have delight installed
;;   :demand ; force loading to occur immediately
;;   :diminish minor-mode-symbol ; if you have diminish installed
;;   :diminish cons-replacement-symbol ; if you have diminish installed
;;   :diminish replacement-string ; if you have diminish installed
;;   :disabled ; disable the package and this declaration as a whole
;;   :ensure t ; install package using elpa if not installed
;;   :ensure package-name ; install package-name using elpa if not installed and different from use-package package-name
;;   :functions function : introduce function for byte compiler
;;   :if conditional ; read following lines only if conditional is t
;;   :init (code to execute before package is loaded)
;;   :load-path "path-containing-the-package"
;;   :load-path (lambda() (dynamically generated path or path list))
;;   :no-require t ; stop package from autoload during compile for symbol declaration
;;   :pin archive : pin package to specified archive
;;   :preface (command to execute at byte-compile time)
;;   :requires package ; same as :if (featurep 'package)
;;   :requires (list of packages)
;;   :unless conditional ; alias for :if (not conditional)
;;   :when conditional ; alias for :if
;;; Code:

;; manually added (no require but included with package-initialize add-to-list)...
;;(require 'linkd) ; allow creation of hyperlinks -- manually installed -- small and interesting

;; melpa install
(if (require 'adaptive-wrap nil "Display wrap indention+indicators.")
    (adaptive-wrap-prefix-mode t)
  (message "Could not require adaptive-wrap to wrap long lines nicely"))

;; ansi-color -- cause emacs to respect color sequences in data
;; @see https://emacs.stackexchange.com/questions/24698/ansi-escape-sequences-in-compilation-mode
(defun ansi-color/color-compile-endless ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))
;; ansi-color -- cause emacs to remove color sequences from data
;; @see https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of REGEXPS."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))
(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")
(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))
(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))
(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))
;;; Note that the second part is actually getting applied to all comint derivatives, not just compilation-mode (It also fixes issues in, e.g. shell). If this is not what you want, then add the hook to compilation-filter-hook instead.
(if (require 'ansi-color)
    (or t ; UNTESTED
        ;; do the following to cause emacs to respect the escape sequences 
        (add-hook 'compilation-filter-hook
                  #'ansi-color/color-compile-endless)
        ;; do the following to filter the sequences from the data
        (add-hook 'comint-output-filter-functions
                  'filter-non-sgr-control-sequences-in-output))
  (message "Cannot cause emacs to interpret ansi-color escape sequences used in data"))

;; melpa install
(unless (require 'bind-key nil "Macros whichs ease key binding.")
  (message "Could not require bind-key which is clearly expected"))

;; built-in
(unless (require 'cl-lib nil "Common lisp library for cl- family of functions.")
  (message "Could not require the cl-lib for some keyboard declarations"))

;; melpa install
(if (require 'cobol-mode nil "Cobol language facer.")
    (setq auto-mode-alist ;; doesn't seem to come with any regexes
          (append '((".cbl" . cobol-mode)) auto-mode-alist))
  (message "Could not require cobol-mode for facing cobol"))

;; melpa install
(if (require 'company nil "Complete Anything.")
 		(global-company-mode t)
 	(message "Could not require company - a prerequesite for flycheck"))

;; built-in
(unless (require 'dash nil "List API.")
 	(message "Could not require dash - not known to be needed"))

;; company-mode is a prerequesite
(if (require 'flycheck nil "Syntax checking.")
		;; (flycheck-verify-setup)' to troubleshoot your Flycheck setup.
		(add-hook 'after-init-hook #'global-flycheck-mode) ; recommended manner to turn on
	(message "Could not flycheck to perform language sytax checking"))

;; melpa install
(if (require 'highlight-parentheses nil "Highlight mupliple levels of parentheses.")
		(global-highlight-parentheses-mode t)
	(message "Could not require much highlight-parentheses to colorize matching parentheses"))

;; melpa install
(unless (require 'htmlize' nil "Convert to HTML")
  (message "Could not include htmlize - Missing functions to convert buffer text and decorations to HTML"))

;; melpa install
(unless (require 'htmltagwrap' nil "Wrap HTML")
  ;; does not have any keybindings
  (message "Could not include htmltagwrap - Missing function htmltagwrap-tag-wrap"))

;; melpa install
(if (require 'iedit nil "Interactive search and edit all matches.")
    (progn
      (when (boundp 'iedit-mode-keymap)
		  (set-keymap-parent iedit-mode-keymap nil)) ; remove tab bindings
      ;; iedit-mode-line isnot actually customizable
      (defvar iedit-mode-line)
      (setq iedit-mode-line
            `(" iedit:"
              (:eval (format ,(propertize "%d/%d" 'face 'compilation-line-number)
                             iedit-occurrence-index (iedit-counter)))))
      ;; iedit-mode-line needs to be installed into minor-mode-alist
      (let ((zAssoc))
           (while (setq zAssoc (assoc 'iedit-mode minor-mode-alist))
             (setq minor-mode-alist (delq zAssoc minor-mode-alist))) ; probably a quicker way to do this
           (nconc minor-mode-alist
                  (list `(iedit-mode ,iedit-mode-line)))))
	(message "Could not require iedit to edit multiple matches via C-S-f"))

;; created by me -- should upload to git-hub in hopes of improvements
(unless (require 'interfaces nil "Declare /etc/network/interfaces syntax highlighting.")
  (message "Could not require interfaces to face /etc/network/interfaces"))

;; melpa install
;; apt-get install virtualenv ;; i guess the parser runs in a virtual environment
;; pip install epc ;; needed for python import epc (too used by jedi)
;; pip install jedi ;; needed for python import jedi (flyspell input generaor?)
;; jedi-mode ;; needed for every python buffer for flyspell interface
(if (require 'jedi nil "Python autocompletion")
		(progn (add-hook 'python-mode-hook 'jedi-mode) ;; activate mode for flyspell interface (during jedi:setup)
					 (add-hook 'python-mode-hook 'jedi:setup) ;; pip install jedi ;; for import jedi (flyspell input generator)
					 (setq jedi:complete-on-dot t))
  (message "Could not require jedi to autocomplete python methods"))

;; melpa install
(if (require 'mutt-mode nil "Major mode for mutt configuration files.")
    (setq auto-mode-alist ;; why don't the other regexes work?
            (append '(("muttrc" . mutt-mode)) '(("mailcap" . mutt-mode)) auto-mode-alist))
  (message "Could not mutt-mode for editing mutt configuration files"))

;; melpa install
(if (require 'persistent-scratch nil "Keep the scratch buffer.")
		(progn
			(customize-set-variable 'persistent-scratch-backup-directory "~/.emacs.d/backup")
			(customize-set-variable 'persistent-scratch-autosave-interval 5 "seconds")
			(persistent-scratch-restore) ; needed or will not restore
			(persistent-scratch-autosave-mode t))
	(message "Could not require persistent-scratch to keep the scratch buffer"))

;; i may have created this mode too
(unless (require 'nftables-mode nil "Formats nftables code.")
  (message "Could not require nftables-mode major mode"))

;; melpa install
(unless (require 'org nil "Organizer.")
  (message "Could not require org-mode for scratch"))

;; melpa install
(unless (require 'realgud nil "Real GNU Debugging.")
  (message "Could not require readgud to debug"))

;; manual copy from emacsWiki.org -- rft is Microsoft proprietary -- don't expect acceptance
(unless (require 'rtf-mode nil "Rich Text Format recognition.")
  (message "Could not require rtf-mode for rtf highlighting"))

;; melpa install
(unless (require 's nil "String API.")
  (message "Could not require s - not known to be needed"))

;; could not find package -- seemed to work when require failed -- caused itself to become available
(unless (require 'saveplace nil "Save file editing position.")
  (message "Could not require saveplace to keep editing positions"))

;;(unless (require 'ses nil "Simple Emacs Spreadsheet.")
;;  (message "Could not require ses for spreadsheets"))

;; melpa install
(if (require 'sudo-edit nil "Allow to open files as another user.")
    (progn
      (defun find-alternative-file-with-sudo ()
        "Should be connected to C-x C-q which normally toggles readonly mode."
        (let ((bname (expand-file-name (or buffer-file-name default-directory)))
              (zPoint (point)))
          (setq bname (or (file-remote-p bname 'localname) (concat "/sudo::" bname)))
          (cl-flet ((server-buffer-done (buffer &optional for-killing) nil))
            (find-alternate-file bname))
          (goto-char zPoint)))
      (defface find-file-root-header-face
        '((t (:foreground "white" :background "red3")))
        "Header lines face for files opened as root.")
      (defun find-file-root-header-warning ()
        "Display a warning in header line. May add to `find-file-hook'."
        (when (string-equal  "root" (file-remote-p (or buffer-file-name default-directory) 'user))
          (let* ((warning "WARNING: EDITING FILE AS ROOT!")
                 (space (+ 6 (- (window-width) (length warning))))
                 (bracket (make-string (/ space 2) ?-))
                 (warning (concat bracket warning bracket)))
            (setq header-line-format (propertize  warning 'face 'find-file-root-header-face)))))
      (add-hook 'find-file-hook 'find-file-root-header-warning)
      (add-hook 'dired-mode-hook 'find-file-root-header-warning))
  (message "Could not require sudo-edit which roots without password via C-x C-q!"))

;; could not find package -- seemed to work when require failed -- caused itself to become available
(unless (require 'undo-fu nil "Undo/Redo.")
  (message "Could not require undo-fu to allow undo"))

;; modified by me -- should offer updates
(unless (require 'vcard nil "Declare vcard syntax highlighting.")
  (message "Could not require vcard containing my syntax expansions"))
