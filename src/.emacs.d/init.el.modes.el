;;
;; init.el.modes.el (included by init.el)
;;
;; (enriched-mode t) ; save text formatting as part of file

(add-hook 'dired-mode-hook (lambda ()
														 "Cause dired-mode to use mono space font."
														 (face-remap-add-relative 'default :family "Monospace")))

(dolist (hook '(after-change-major-mode-hook))
  (add-hook hook (lambda ()
									 "Keep flyspell-mode active"
									 (flyspell-mode 1))))

(blink-cursor-mode t) ; global ; cursor blinker
(cua-mode t) ; global ;do not replace C-x ; C-c C-v manually overloaded
(delete-selection-mode t) ; global ; selected text replacer
(desktop-save-mode t) ; global ; desktop saver
(electric-indent-mode t) ; global ; auto indentation
(flyspell-prog-mode) ; local ; spell checker (prog for comments and strings only)
(global-font-lock-mode t) ; text fontifier
(global-hl-line-mode t) ; current line highlighter
(global-superword-mode 0) ; word separator reducer
(global-visual-line-mode t) ; line wrapping and up/down by visual lines
(icomplete-mode t) ; global ; available completion displayer
(menu-bar-mode -1) ; global ; disable the menu bar
(save-place-mode t) ; global ; save point for later recall in save-place-file
(scroll-bar-mode 0) ; global ; scroll bar displayer
(show-paren-mode t) ; global ; matching pair indicator
(toggle-scroll-bar -1) ; global ; disable the scroll bars
(tool-bar-mode -1) ; global ; remove tool bar
(transient-mark-mode t) ; global ; cause to operate on the region if active
(visual-line-mode 0) ; local ; visual word wrapping
(winner-mode t) ; global ; adds changes to windows to the undo stack 
