;;; init-gpg.el --- gpg setting

;; Put this in ~/.gnupg/gpg-agent.conf:
;; allow-emacs-pinentry
;; allow-loopback-pinentry

;;; Commentary:
;; 

;;; Code:

(setq epa-pinentry-mode 'loopback)
(setq auth-sources '("~/.authinfo.gpg"))

(provide 'init-gpg)

;;; init-gpg.el ends here
