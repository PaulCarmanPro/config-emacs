;;; init.el.search.el (included by init.el)
;;
;;; Commentary:
;; search around the world buddy (how can people be so lame?)

;;; Code:

;; prevent need to backspace twice when trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-auto-wrap activate)
		"Automatically wrap search in event of search failure."
	 (unless isearch-success
			; do not infinite loop for repeated failure
		 (ad-disable-advice 'isearch-search 'after 'isearch-auto-wrap)
		 (ad-activate 'isearch-search)
		 (isearch-repeat (if isearch-forward 'forward))
		 (ad-enable-advice 'isearch-search 'after 'isearch-auto-wrap)
		 (ad-activate 'isearch-search)))

;;; iedit no longer in use ?? see init.el.requre.el ;;;

(defadvice iedit-next-occurrence (after iedit-next-auto-wrap activate)
		"Automatically wrap search in event of search failure."
	 (unless iedit-forward-success
			; do not infinite loop for repeated failure
		 (ad-disable-advice 'iedit-next-occurrence 'after 'iedit-next-auto-wrap)
		 (ad-activate 'iedit-next-occurrence)
		 (iedit-next-occurrence)
		 (ad-enable-advice 'iedit-next-occurrence 'after 'iedit-next-auto-wrap)
		 (ad-activate 'iedit-next-occurrence)))

(defadvice iedit-prev-occurrence (after iedit-prev-auto-wrap activate)
		"Automatically wrap search in event of search failure."
	 (unless iedit-forward-success
			; do not infinite loop for repeated failure
		 (ad-disable-advice 'iedit-prev-occurrence 'after 'iedit-prev-auto-wrap)
		 (ad-activate 'iedit-prev-occurrence)
		 (iedit-prev-occurrence)
		 (ad-enable-advice 'iedit-prev-occurrence 'after 'iedit-prev-auto-wrap)
		 (ad-activate 'iedit-prev-occurrence)))

