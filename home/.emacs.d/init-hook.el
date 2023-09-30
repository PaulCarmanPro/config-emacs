;;; init.el-hook.el --- included by init.el

;;; Commentary:

;; (list-packages) ; wait a long time for package list to load and open
;; installed packages exist in ~/.emacs.d/elpa
;;   d = mark for deletion
;;   i = mark for install
;;   u = unmark
;;   x = execute marks
;;   r = refresh list
;;   w = open website
;;   U = mark all that need update
;;   <del> = remove mark
;; package-activated-list contains names of currently activated packages.

;; ~/.emacs.d/elpa files downloaded/updated via require via the package system.
;; ~/.emacs.d/lisp files manually downnnnnloaded/created and need to be kept.

(require 'package) ; the built-in package handler
(package-initialize) ; auto-load packages -- why do I need or want this?
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/")) ; more packages
(add-to-list 'load-path "~/.emacs.d/elpa/") ; search for packages here
(add-to-list 'load-path "~/.emacs.d/lisp/") ; search for packages here
(add-to-list 'load-path "~/.emacs.d/lisp/vcard-0.1/") ; load-path is not recursive
(load-file "~/.emacs.d/init-hook-major.el") ; load major modes via 'require'
(load-file "~/.emacs.d/init-hook-minor.el") ; load minor modes via 'require'
;;; Code: These 'require' commands load functions and/or macros

;; /usr/local/share/emacs/#/lisp/ansi-color
;; @see https://emacs.stackexchange.com/questions/24698/ansi-escape-sequences-in-compilation-mode
;; @see https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/
(if (require 'ansi-color nil "Functoins to respect color escape sequences in data." )
    (progn
      (defun ansi-color/color-compile-endless ()
        "Colorize from `compilation-filter-start' to `point'."
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region
           compilation-filter-start (point))))
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
      ;; do the following to cause emacs to respect the escape sequences
      (add-hook 'compilation-filter-hook
                #'ansi-color/color-compile-endless)
      ;; do the following to filter the sequences from the data
      (add-hook 'comint-output-filter-functions
                'filter-non-sgr-control-sequences-in-output))
  (message "Cannot require ansi-color"))

;; ~/.emacs.d/elpa/bind-key
(unless (require 'bind-key nil "Macros for key binding.")
  (message "Could not require bind-key used in ~/.emacs.d/init-keyboard.el"))

;; /usr/local/share/emacs/#/lisp/emacs-lisp/cl-lib
(unless (require 'cl-lib nil "Common lisp library of cl-functions.")
  (message "Could not require the cl-lib"))

;; ~/.emacs.d/elpa/htmlize
(unless (require 'htmlize' nil "Functions to convert text and decorations into HTML")
  ;; no keybindings
  (message "Could not require htmlize"))

;; ~/.emacs.d/elpa/htmltaagwrap
(unless (require 'htmltagwrap' nil "Functions to wrap a chuck of HTML in tags")
  ;; no keybindings
  (message "Could not require htmltaagwrap"))

;; ~/.emacs.d/elpa/mbsync
(unless (require 'mbsync nil "Functions to fetch mail.")
  (message "Could not mbsync"))

;; ~/.emacs.d/elpa/persistent-scratch
(if (require 'persistent-scratch nil "Keep the scratch buffer.")
      (progn
         (customize-set-variable 'persistent-scratch-backup-directory "~/.emacs.d/backup")
         (customize-set-variable 'persistent-scratch-autosave-interval 5 "seconds")
         (persistent-scratch-restore) ; needed or will not restore
         (persistent-scratch-autosave-mode t))
   (message "Could not require persistent-scratch"))

;; ~/.emacs.d/elpa/realgud
(unless (require 'realgud nil "Real GNU Debugging.")
  (message "Could not require readgud"))

;; ~/.emacs.d/elpa/s -- UNUSED
(unless (require 's nil "String function API.")
  (message "Could not require s"))

;; /usr/local/share/emacs/#/saveplace
(unless (require 'saveplace nil "Save file editing position.")
  (message "Could not require saveplace"))

;; init.el.sudo.el (included by init.el)
(defadvice vSudo-find-file (after find-file-sudo activate)
  "Allow root files to be edited. Find file via /sudo:root@hostname: or /sudo:hostname: if necessary."
  (unless (and buffer-file-name (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@alsa:" buffer-file-name))))

;; ~/.emacs.d/elpa/sudo-edit
(if (require 'sudo-edit nil "Sudo editing abilities.")
    (progn
      (defun find-alternative-file-with-sudo ()
        "Please connect to key which toggles readonly mode."
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
  (message "Could not require sudo-edit"))

;; ~/.emacs.d/elpa/undo-fu
(unless (require 'undo-fu nil "Undo/Redo.")
  (message "Could not require undo-fu"))

;; @see https://github.com/jwiegley/use-package
;;
;; this was created for lisp-mode
;; @see https://anarc.at/blog/2022-04-27-lsp-in-debian/
;; it brings language help into emacs, but is quite complex
;;
;;;; (eval-when-compile
;;;;   (if (require 'use-package)
;;;;       (progn (setq use-package-hook-name-suffix nil) ; do not add -hook to hook varible
;;;;              (setq use-package-always-ensure t)) ; assume :ensure t for every use-package
;;;;     (message "!!! Serious problem: Could not require use-package !!!")))
;;
;; NOT USED but it is interesting...
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))
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
;;   :ensure t ; install package using elpa if not installed (does not cause automatic updates)
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

