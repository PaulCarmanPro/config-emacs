;;; init.el - emacs init file
;;
;;; Commentary:
;; NO -*- lexical-binding: nil; -*- nim-ringset-with-car NEEDS lexical-binding
;; (list-colors-display) ;; show all named colors
;; (list-faces-display) ;; show all named faces

;;; Code:
(server-start) ; allow emacsclient to locate me

;; rest of init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/init-require.el")
(load-file "~/.emacs.d/init-modes.el")
(load-file "~/.emacs.d/init-keyboard.el")
(load-file "~/.emacs.d/init-search.el")
(load-file "~/.emacs.d/init-modeline.el")
(load-file "~/.emacs.d/init-sudo.el")

;; auto-save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-save-list-file-prefix nil ;; prevent ~/.emacs/auto-save-list/ ;; use recover-file to recover auto-save file
			backup-directory-alist `((".*" . "~/.emacs.d/backup")) ;; where to store backups (instead of locally)
			;;		auto-save-file-name-transforms `(".*" "~/.emacs.d/auto-save" t)
      debug-on-quit t ; debug due to C-g
 delete-old-versions t ; file
      kept-new-versions 6 ; file
      kept-old-versions 2 ; file
      version-control t ; file
      debugxev-on-error t) ; debug

;; enable abilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; wide editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq truncate-partial-width-windows nil)
(toggle-truncate-lines 1)
(visual-line-mode 0)

;; camelCase aspell checking wanted -- promised in version 0.60.8 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ispell-program-name "aspell")
(setq-default ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))

;; make easier to remember
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst compile-directory (byte-recompile-directory))
(defsubst compile-file (byte-recompile-file))

;; auto-edit must be part of init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adaptive-wrap-extra-indent 1)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auth-source-save-behavior nil)
 '(cua-enable-cursor-indications t)
 '(cua-normal-cursor-color '(bar . "cyan1"))
 '(cua-overwrite-cursor-color '(hollow . "yellow"))
 '(cua-paste-pop-rotate-temporarily t)
 '(cua-read-only-cursor-color '(hollow . "yellow"))
 '(cursor-in-non-selected-windows nil)
 '(cursor-type '(bar . 4))
 '(debug-all-defs nil)
 '(debug-on-error t)
 '(desktop-load-locked-desktop t)
 '(dired-listing-switches
   "--all --classify --format=long --group-directories-first --human-readable")
 '(edebug-save-windows nil)
 '(flycheck-sh-bash-args '("-O" "extglob"))
 '(font-lock-maximum-decoration t)
 '(frame-resize-pixelwise t)
 '(gdb-many-windows t)
 '(global-display-line-numbers-mode t)
 '(highlight-parentheses-background-colors nil)
 '(highlight-parentheses-colors '("firebrick1" "IndianRed1" "gold" "white"))
 '(hscroll-margin 10)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(initial-major-mode 'org-mode)
 '(lazy-highlight-initial-delay 0)
 '(lazy-highlight-max-at-a-time 1)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-progressive-speed t)
 '(mouse-wheel-scroll-amount '(2 ((shift) . 1)))
 '(narrow-to-defun-include-comments t t)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(go-mode golint cobol-mode htmlize htmltagwrap mutt-mode lsp-mode s undo-fu org sudo-edit realgud persistent-scratch multiple-cursors jedi iedit highlight-parentheses flycheck company bind-key adaptive-wrap))
 '(python-indent-offset 3)
 '(realgud:pdb-command-name "python3.10 -m pdb")
 '(scroll-conservatively 0)
 '(scroll-down-aggressively 0.01)
 '(scroll-margin 2)
 '(scroll-up-aggressively 0.01)
 '(sh-basic-offset 3)
 '(show-paren-ring-bell-on-mismatch t)
 '(show-paren-style 'mixed)
 '(tab-always-indent 'complete)
 '(tab-width 3)
 '(tex-dvi-view-command "xdvi -bg black -fg bisque -fullscreen")
 '(truncate-lines t)
 '(use-file-dialog nil)
 '(vc-follow-symlinks t))

;; set font for emoji
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
 ;; GNU Emacs Removes Color Emoji Support on the Mac
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 ;;
 )
(featurep 'cairo) ;; currently nil ;; must configure --with-cario to use emoji

;; (list-faces-display)
;; (what-cursor-position t) "M-f"
;; (custonize-face)
;; auto-edit must be part of init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "#4fCC80" :height 200 :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(compilation-line-number ((t (:inherit line-number :foreground "yellow" :height 1.3))))
 '(compilation-mode-line-exit ((t (:foreground "SpringGreen4"))))
 '(compilation-mode-line-fail ((t (:inherit line-number :foreground "yellow"))))
 '(compilation-mode-line-run ((t (:foreground "dark goldenrod"))))
 '(cursor ((t (:foreground "black" :background "cyan"))))
 '(custom-button ((t (:background "#007799" :foreground "black" :box (:line-width 2 :style released-button) :weight extra-bold))))
 '(error ((t (:foreground "salmon1"))))
 '(flyspell-incorrect ((t (:underline "orange red"))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#ffbf24"))))
 '(font-lock-comment-face ((t (:foreground "orange" :height 1.1 :family "monofur"))))
 '(font-lock-constant-face ((t (:foreground "pale turquoise"))))
 '(font-lock-function-name-face ((t (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "cyan1"))))
 '(font-lock-negation-char-face ((t (:foreground "pale violet red"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "deep pink"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "bisque"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:foreground "aquamarine"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow green"))))
 '(fringe ((t (:foreground "firebrick1"))))
 '(gnus-header-content ((t (:weight normal :foreground "yellow green"))))
 '(gnus-header-from ((t (:foreground "pale green"))))
 '(gnus-header-name ((t (:foreground "dark sea green"))))
 '(gnus-header-newsgroups ((t (:foreground "dark khaki"))))
 '(gnus-header-subject ((t (:foreground "pale turquoise"))))
 '(highlight ((t (:background "dark red" :foreground "#00CCCC"))))
 '(highlight-parentheses-highlight ((t (:foreground "dodger blue"))) t)
 '(hl-line ((t (:background "#001018"))))
 '(info-header-node ((t (:inherit info-node))))
 '(isearch ((t (:inherit lazy-highlight :box (:line-width (15 . 1) :color "yellow" :style released-button)))))
 '(lazy-highlight ((t (:background "#4bc7cf" :foreground "black" :box (:line-width (2 . 2) :color "yellow" :style released-button)))))
 '(line-number ((t (:inherit default :foreground "magenta" :height 0.8 :family "Monoisome Tight"))))
 '(line-number-current-line ((t (:inherit line-number :foreground "white" :box (:line-width 1 :color "white")))))
 '(link ((t (:underline t :foreground "cyan"))))
 '(link-visited ((t (:underline t :foreground "dark cyan"))))
 '(linkd-tag-name ((t (:foreground "gold" :underline t))))
 '(mc/cursor-bar-face ((t (:background "goldenrod3" :height 1))))
 '(mc/cursor-face ((t (:background "goldenrod4"))))
 '(message-cited-text-1 ((t (:foreground "SpringGreen3"))))
 '(message-header-cc ((t (:foreground "yellow green"))))
 '(message-header-name ((t (:foreground "dark turquoise"))))
 '(message-header-other ((t (:foreground "dark khaki"))))
 '(message-header-subject ((t (:foreground "pale turquoise"))))
 '(message-header-to ((t (:foreground "pale green"))))
 '(message-separator ((t (:foreground "deep sky blue"))))
 '(mode-line ((t (:inherit default :background "#281810" :foreground "#ff9f44" :box (:line-width (1 . -1) :style released-button)))))
 '(mode-line-inactive ((t (:inherit modeline :background "#100804" :foreground "#8f5f24" :box (:line-width (1 . -1) :style released-button)))))
 '(realgud-backtrace-number ((t (:foreground "dark cyan" :weight bold))))
 '(realgud-bp-disabled-face ((t (:foreground "dark slate gray"))))
 '(realgud-bp-enabled-face ((t (:inherit error))))
 '(realgud-bp-line-disabled-face ((t (:underline "salmon"))))
 '(realgud-bp-line-enabled-face ((t (:underline "SpringGreen3"))))
 '(realgud-file-name ((t (:foreground "dark khaki"))))
 '(realgud-line-number ((t (:foreground "dark cyan"))))
 '(realgud-overlay-arrow1 ((t (:foreground "SpringGreen3"))))
 '(realgud-overlay-arrow2 ((t (:foreground "white"))))
 '(realgud-overlay-arrow3 ((t (:foreground "wheat"))))
 '(region ((t (:foreground "white" :background "dark green"))))
 '(secondary-selection ((t (:background "dark slate gray"))))
 '(sh-heredoc ((t (:foreground "#bbbb00" :weight bold))))
 '(shadow ((t (:foreground "grey70"))))
 '(show-paren-match ((t (:foreground "white"))))
 '(success ((t (:foreground "yellow green"))))
 '(tool-bar ((t (:background "#0099cc" :foreground "black" :box (:line-width 1 :style released-button) :weight bold))))
 '(vc-follow-symlinks t)
 '(warning ((t (:foreground "orange"))))
 '(widget-field ((t (:background "#281810" :box (:line-width 2 :color "orange" :style released-button))))))

(put 'scroll-left 'disabled nil)
