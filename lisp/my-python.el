;;; my-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:

(require 'lsp-python-ms)
(require 'dap-python)

(setq lsp-python-ms-auto-install-server t)

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq-local lsp-diagnostics-provider :none)
	    (lsp)))

(setq lsp-python-ms-python-executable-cmd "python3")


(provide 'my-python)

;;; my-python.el ends here
