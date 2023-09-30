;;; init-mode-minor.el --- included by init.el
;;; Code: 'require' commands which load minor modes

;; ~/.emacs.d/elpa/adaptive-wrap
(if (require 'adaptive-wrap nil "Display line wrap indention/indicators.")
    (adaptive-wrap-prefix-mode t)
  (message "Could not require adaptive-wrap"))

;; ~/.emacs.d/elpa/company
(if (require 'company nil "Complete Anything.")
    (global-company-mode t)
  (message "Could not require company - a prerequesite for flycheck"))

;; /usr/local/share/emacs/#/lisp/emulation/cua
(if (require 'cua-base nil "Common User Application minor mode.")
    (cua-mode t) ; global ;do not replace C-x ; C-c C-v manually overloaded
   (message "Could not require cua-base"))

;; /usr/local/share/emacs/#/lisp/delsel
(if (require 'delsel nil "Delete selection minor mode.")
    (delete-selection-mode t) ; global
   (message "Could not require delsel"))

;; /usr/local/share/emacs/#/lisp/desktop.el
(if (require 'desktop nil "Desktop operations")
    (desktop-save-mode t) ; global ; desktop saver
  (message "Could not require desktop"))

;; /usr/local/share/emacs/#/lisp/electric.el
(if (require 'electric nil "Window maker and Command loop for `electric' modes")
    (electric-indent-mode t) ; global ; auto indentation
  (message "Could not require electric"))

;; built-in /8/emacs/src/fns.c
(if (require 'flyspell nil "Spell checker")
    (flyspell-prog-mode) ; local ; spell checker (prog for comments and strings only)
  (message "Could not require flyspell"))

;; /usr/local/share/emacs/#/lisp/font-core.el
(if (require 'font-core nil "Core interface to font-lock")
    (global-font-lock-mode t) ; text fontifier
  (message "Could not require font-core"))

;; company-mode is a prerequesite
(if (require 'flycheck nil "Syntax checking.")
      ;; (flycheck-verify-setup)' troubleshoot Flycheck setup.
      (add-hook 'after-init-hook #'global-flycheck-mode) ; recommended manner
   (message "Could not flycheck"))

;; /usr/local/share/emacs/#/lisp/textmodes/flyspell
(if (require 'flyspell nil "Spell checker.")
    (dolist (hook '(after-change-major-mode-hook))
      (add-hook hook (lambda ()
                       "Keep flyspell-mode active"
                       (flyspell-mode 1))))
   (message "Could not require flyspell"))

;; /usr/local/share/emacs/#/lisp/frame
(if (require 'frame nil "Frame management")
    (blink-cursor-mode t) ; global ; cursor blinker/stopper
  (message "Could require frame"))

;; /usr/local/share/emacs/#/lisp/hl-line.el
(if (require 'hl-line nil "Highlight current line")
    (global-hl-line-mode t) ; current line highlighter
  (message "Could not require hl-line"))

;; ~/.emacs.d/elpa/highlight-parentheses
(if (require 'highlight-parentheses nil "Highlight mupliple levels of parentheses.")
      (global-highlight-parentheses-mode t)
   (message "Could not require highlight-parentheses"))

;; ~/.emacs.d/elpa/iedit
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
   (message "Could not require iedit"))

;; /usr/local/share/emacs/#/lisp/icomplete.el
(if (require 'icomplete nil "Highlight current line")
    (icomplete-mode t) ; global ; available completion displayer
  (message "Could not require icomplete"))

;; ~/.emacs.d/elpa/jedi
;; apt-get install virtualenv ;; i guess the parser runs in a virtual environment
;; pip install epc ;; needed for python import epc (too used by jedi)
;; pip install jedi ;; needed for python import jedi (flyspell input generaor?)
;; jedi-mode ;; needed for every python buffer for flyspell interface
(if (require 'jedi nil "Python autocompletion")
      (progn (add-hook 'python-mode-hook 'jedi-mode) ;; activate mode for flyspell interface (during jedi:setup)
                (add-hook 'python-mode-hook 'jedi:setup) ;; pip install jedi ;; for import jedi (flyspell input generator)
                (setq jedi:complete-on-dot t))
  (message "Could not require jedi"))

;; /usr/local/share/emacs/#/lisp/menu-bar.el
(when (require 'menu-bar nil "Show menu bar")
    (menu-bar-mode -1)) ; global ; disable the menu bar

;; /usr/local/share/emacs/#/lisp/paren.el
(if (require 'paren nil "Parentheses counter")
    (show-paren-mode t) ; global ; matching pair indicator
  (message "Could not require paren"))

;; /usr/local/share/emacs/#/lisp/saveplace.el
(if (require 'saveplace nil "Save/recall point in save-place-file")
    (save-place-mode t) ; global ; save point for later recall in save-place-file
  (message "Could not require saveplace"))

;; /usr/local/share/emacs/#/lisp/scroll-bar.el
(when (require 'scroll-bar nil "Display scroll bars")
  (scroll-bar-mode 0) ; global ; remove scroll bar
  (toggle-scroll-bar -1)) ; global ; disable the scroll bars

;; /usr/local/share/emacs/#/lisp/simple.el
(if (require 'simple nil "Highlight current line")
    (progn
      (global-visual-line-mode t) ; line wrapping and up/down by visual lines
      (transient-mark-mode t) ; global ; cause to operate on the region if active
      (visual-line-mode 0)) ; local ; visual word wrapping
  (message "Could not require simple"))

;; /usr/local/share/emacs/#/lisp/progmodes/subword.el
(if (require 'subword nil "Highlight current line")
    (global-superword-mode 0) ; word separator reducer
  (message "Could not require subword"))

;; /usr/local/share/emacs/#/lisp/tool-bar.el
(when (require 'tool-bar nil "Parentheses counter")
    (tool-bar-mode -1)) ; global ; remove tool bar

;; /usr/local/share/emacs/#/lisp/winner.el
(if (require 'winner nil "Parentheses counter")
    (winner-mode t) ; global ; adds changes to windows to the undo stack
  (message "Could not require winner"))
