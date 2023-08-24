;;
;; interfaces.el
;;
;; create a mode for syntax highlighting of /etc/network/interfaces

(defface interfaces-declaration-face
  '((t :inherit font-lock-keyword-face))
  "Face for highlighting interface declaration starter."
  :group 'interfaces)

(defface interfaces-stanza-face
  '((t :inherit font-lock-builtin-face))
  "Face for highlighting stanza starters."
  :group 'interfaces)

(defface interfaces-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for highlighting interface names."
  :group 'interfaces)

(defface interfaces-family-face
  '((t :inherit font-lock-constant-face))
  "Face for highlighting address family names."
  :group 'interfaces)

(defface interfaces-method-face
  '((t :inherit font-lock-constant-face))
  "Face for highlighting access method names."
  :group 'interfaces)

(defface interfaces-static-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting access method names."
  :group 'interfaces)

(defface interfaces-bond-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting access method names."
  :group 'interfaces)

(defface interfaces-dynamic-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting access method names."
  :group 'interfaces)

(defface interfaces-wireless-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting access method names."
  :group 'interfaces)

(defface interfaces-command-face
  '((t :inherit font-lock-function-name-face))
  "Face for highlighting access method names."
  :group 'interfaces)

(defvar interfaces-font-lock-keywords
  '(("^[[:space:]]*iface[[:space:]]" . 'interfaces-declaration-face) ;; iface  <interface>  <address_family>  <method>
		("^[[:space:]]*\\(auto\\|allow-hotplug\\)[[:space:]]" . (1 'interfaces-stanza-face)) ;; auto|allow-hotplug <interface>
		("^[[:space:]]*\\(iface\\|auto\\|allow-hotplug\\)[[:space:]]+\\([[:word:]]+\\)" . (2 'interfaces-name-face))
		("^[[:space:]]*iface[[:space:]]+[[:word:]]+[[:space:]]+\\(inet\\|inet6\\)[[:space:]]" . (1 'interfaces-family-face))
		("^[[:space:]]*bridge_ports[[:space:]]+\\(\\([[:word:]]+\\([[:space:]]+\\|$\\)\\)+\\)" . (1 'interfaces-name-face))
		("^[[:space:]]*iface[[:space:]]+\\([[:word:]]+[[:space:]]+\\)\\{2\\}\\(dhcp\\|manual\\|loopback\\|static\\)[[:space:]]*$" . (2 'interfaces-method-face))
		("^[[:space:]]*\\(address\\|netmask\\|gateway\\|broadcast\\)[[:space:]]" . (1 'interfaces-static-face))
		("^[[:space:]]*\\(bond-master\\|bond-primary\\|bond-slaves\\|bond-mode\\|bond-miimon\\|bond-downdelay\\|bond-updelay\\)[[:space:]]" . (1 'interfaces-bond-face))
		("^[[:space:]]*\\(dns-nameservers\\|bridge_ports\\)[[:space:]]" . (1 'interfaces-dynamic-face))
		("^[[:space:]]*\\(wpa-ssid\\|wpa-bssid\\|wpa-psk\\)[[:space:]]" . (1 'interfaces-wireless-face))
		("^[[:space:]]*\\(pre-up\\|up\\|post-up\\|pre-down\\|down\\|post-down\\)[[:space:]]" . (1 'interfaces-command-face))
		))

(setq interfaces-mode-syntax-table
			(let ( (synTable (make-syntax-table)))
				;; Bash style comment # until eol
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
      synTable))

;;;###autoload
(define-derived-mode interfaces-mode text-mode "interfaces"
  "Major mode for viewing `/etc/network/interfaces'."
	;; syntax-table interfaces-mode-syntax-table ;; not needed because name is formatted correctly
	(setq font-lock-defaults '(interfaces-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\interfaces\\'" . interfaces-mode))

(provide 'interfaces)

