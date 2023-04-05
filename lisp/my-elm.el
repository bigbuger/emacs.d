;;; my-elm.el --- elm lang setting

;;; Commentary:
;;

;;; Code:

(require 'elm-mode)
(require 'lsp-mode)

;; install elm lsp via lsp-install-server
(add-hook 'elm-mode-hook
	  (lambda ()
	    (lsp-deferred)))

(provide 'my-elm)

;;; my-elm.el ends here
