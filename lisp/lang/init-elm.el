;;; init-elm.el --- elm lang setting

;;; Commentary:
;;

;;; Code:

(require 'elm-mode)
(require 'lsp-mode)

;; install elm lsp via lsp-install-server
(add-hook 'elm-mode-hook
	  (lambda ()
	    (lsp-deferred)))

(provide 'init-elm)

;;; init-elm.el ends here
