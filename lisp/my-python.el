;;; my-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:


;; pip3 install pyright
;;(require 'lsp-pyright)
(require 'dap-python)


(setq python-shell-interpreter "python3")
;;(setq lsp-pyright-python-executable-cmd "python3")

;; fixme pyright has a bug in mac, pythonPath is wrong see https://github.com/emacs-lsp/lsp-pyright/issues/65
(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'mspyls)
  (add-to-list 'lsp-disabled-clients 'pyright))

(add-hook 'python-mode-hook
	  (lambda ()
	    ;;(setq-local lsp-diagnostics-provider :none) ;; disable lsp flycheck
	    (lsp)))

(provide 'my-python)

;;; my-python.el ends here
