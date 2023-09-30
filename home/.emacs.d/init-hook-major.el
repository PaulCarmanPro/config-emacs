;; ~/.emacs.d/init-hook-major.el
;;; Code: 'require' commands which load major modes

;; ~/.emacs.d/elpa/cobol-mode
(if (require 'cobol-mode nil "Face COBOL language.")
    (setq auto-mode-alist ;; doesn't seem to come with any regexes
          (append '((".cbl" . cobol-mode)) auto-mode-alist))
  (message "Could not require cobol-mode"))

;; /usr/local/share/emacs/#/dired
(if (require 'dired nil "Directory Mode.")
    (add-hook 'dired-mode-hook
              (lambda ()
                "Cause dired-mode to use mono space font."
                (face-remap-add-relative 'default :family "Monospace")))
  (message "Could not dired"))

;; ~/.emacs.d/lisp/interfaces.el by Paul Carmam
(unless (require 'interfaces nil "Syntax highlighting for /etc/network/interfaces.")
  (message "Could not require interfaces"))

;; ~/.emacs.d/elpa/mutt-mode
(if (require 'mutt-mode nil "Face mutt configuration files.")
    (add-to-list 'auto-mode-alist '("muttrc" . mutt-mode))
    (add-to-list 'auto-mode-alist '("mailcap" . mutt-mode))
  (message "Could require mutt-mode"))

;; ~/.emacs.d/lisp/nftables-mode.el - modified by Paul Carmin
;; originall from https://elpa.gnu.org/packages/nftables-mode.html.
(unless (require 'nftables-mode nil "Face nftables configuration files.")
  (message "Could not require nftables-mode"))

;; ~/.emacs.d/elpa/org -- YUCK
(unless (require 'org nil "Organizer.")
  (message "Could not require org-mode"))

;; ~/.emacs.d/lisp/rtf-mode.el
;; emacsWiki.org -- Microsoft proprietary -- don't expect acceptance
(unless (require 'rtf-mode nil "Rich Text Format recognition.")
  (message "Could not require rtf-mode"))

;; /usr/local/share/emacs/#/text-mode
(if (require 'text-mode nil "Basic text editing mode where long lines wrap...")
    (progn
      (defface text-face
        '((t :family "LMRoman8" :foreground "white" :slant oblique))
        "Basic face hooked into `text-mode`."
        :group 'basic-faces)
      (add-hook 'text-mode-hook
                (lambda() "Change the default text-mode face"
                  (interactive)
                  (buffer-face-set 'text-face))))
  (message "Counld not require text-mode"))

;; ~/.emacs.d/lisp/vcard-0.1 - by Paul Carman
(unless (require 'vcard nil "Face vcard files.")
  (message "Could not require vcard"))
