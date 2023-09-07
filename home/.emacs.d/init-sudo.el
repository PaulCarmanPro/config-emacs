;;
;; init.el.sudo.el (included by init.el)
;;
;; need to ^X^Q  to change write ability of file to attempt sudo
;;
(defadvice vSudo-find-file (after find-file-sudo activate)
  "Allow root files to be edited. Find file via /sudo:root@hostname: or /sudo:hostname: if necessary."
  (unless (and buffer-file-name (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@alsa:" buffer-file-name))))

