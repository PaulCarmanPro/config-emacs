;;; vcard.el --- Package for handling vCard files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>

;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains `vcard-mode', for viewing vCard files.  Other
;; files in this package contain functions for parsing and writing
;; vCard data.

;;; Code:

(defface vcard-begin-end-face
  '((t :inherit shadow))
  "Face for highlighting BEGIN:VCARD and END:VCARD."
  :group 'vcard)

(defface vcard-name-face
  '((t :inherit info-header-node))
  "Face for highlighting name."
  :group 'vcard)

(defface vcard-note-face
  '((t :inherit font-lock-doc-face))
  "Face for highlighting note."
  :group 'vcard)

(defface vcard-parameter-key-face
  '((t :inherit font-lock-comment-face))
  "Face for highlighting parameter keys."
  :group 'vcard)

(defface vcard-parameter-value-face
  '((t :inherit font-lock-type-face))
  "Face for highlighting parameter values."
  :group 'vcard)

(defface vcard-property-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting property names."
  :group 'vcard)

(defface vcard-telephone-digit-face
  '((t :inherit font-lock-comment-face))
  "Face for highlighting telephone digits."
  :group 'vcard)

(defvar vcard-font-lock-keywords
	;; a newline followed by a space is a line continuance in vcard
	;; -- these search patters do NOT consider vcard line continuance
  '( ; keywords go here -- logically should be "BEGIN:VCARD" "END:VCARD"
		("BEGIN:VCARD" . 'vcard-begin-end-face)
		("END:VCARD" . 'vcard-begin-end-face)
		("^[^ \t;:]+" . 'vcard-property-face)
		;; wanting to highlight names -- not the syntax semi-colons
		("^\\([F]?N\\|ORG\\)[:;]\\([^;\n]*\\)" (2 'vcard-name-face))
		("^\\([F]?N\\|ORG\\)[:;]\\([^;\n]*;+\\)\\{1\\}\\([^;\n]*\\)"
		 (3 'vcard-name-face))
		("^\\([F]?N\\|ORG\\)[:;]\\([^;\n]*;+\\)\\{2\\}\\([^;\n]*\\)"
		 (3 'vcard-name-face))
		("^\\([F]?N\\|ORG\\)[:;]\\([^;\n]*;+\\)\\{3\\}\\([^;\n]*\\)"
		 (3 'vcard-name-face))
		;; wanting to highlight only the digits of telephone numbers
		("^TEL[;:][^0-9\n]*\\([0-9]+\\)" (1 'vcard-telephone-digit-face))
		("^TEL[;:]\\([^0-9\n]*[0-9]+\\)\\{1\\}[^0-9\n]*\\([0-9]+\\)"
		 (2 'vcard-telephone-digit-face))
		("^TEL[;:]\\([^0-9\n]*[0-9]+\\)\\{2\\}[^0-9\n]*\\([0-9]+\\)"
		 (2 'vcard-telephone-digit-face))
		("^TEL[;:]\\([^0-9\n]*[0-9]+\\)\\{3\\}[^0-9\n]*\\([0-9]+\\)"
		 (2 'vcard-telephone-digit-face))
		("^NOTE[;:]\\(.*\\)" (1 'vcard-note-face))
    (";\\([^=\n]+\\)=" (1 'vcard-parameter-key-face))
    ("=\\([^;:\n]+\\)[;:]" (1 'vcard-parameter-value-face))
		))

;;;###autoload
(define-derived-mode vcard-mode text-mode "vCard"
  "Major mode for viewing vCard files."
  (turn-off-auto-fill)
	(goto-address-mode)
	(setq goto-address-url-regexp ; remove highlighting of label TEL: as an address
				(replace-regexp-in-string "tel:\\\\\|" "" goto-address-url-regexp))
  (set (make-local-variable 'paragraph-start) "BEGIN:VCARD")
  (setq font-lock-defaults '(vcard-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.[Vv][Cc][Ff]\\'" . vcard-mode))

(provide 'vcard)
;;; vcard.el ends here
