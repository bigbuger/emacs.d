;;; my-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:

;;(require 'lsp-python-ms)
;; pip3 install pyright
(require 'lsp-pyright)
(require 'dap-python)

;;(setq lsp-python-ms-auto-install-server t)
;;(setq lsp-python-ms-python-executable-cmd "python3")

(setq python-shell-interpreter "python3")

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'mspyls))

(add-hook 'python-mode-hook
	  (lambda ()
	    ;;(setq-local lsp-diagnostics-provider :none) ;; disable lsp flycheck
	    (lsp)))

(provide 'my-python)

;;; my-python.el ends here
