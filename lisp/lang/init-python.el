;;; init-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:


;; pip3 install pyright
(require 'lsp-pyright)
(require 'dap-python)


(setq python-shell-interpreter "python3"
      python-shell-completion-native-disabled-interpreters '("python3")
      lsp-pyright-python-executable-cmd "python3"
      dap-python-executable "python3")


(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'mspyls))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq-local lsp-diagnostics-provider :none) ;; disable lsp flycheck
	    (lsp-deferred)))
(setq dap-python-debugger 'debugpy)

(provide 'init-python)

;;; init-python.el ends here
