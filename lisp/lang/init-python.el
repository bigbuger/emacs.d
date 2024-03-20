;;; init-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:


;; pip3 install pyright
;;(require 'lsp-pyright)
(require 'dap-python)


(setq python-shell-interpreter "python3"
      python-shell-completion-native-disabled-interpreters '("python3")
      dap-python-executable "python3")

;; use python-lsp-server
;; pip3 install 'python-lsp-server[all]'
(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'mspyls)
  (add-to-list 'lsp-disabled-clients 'pyright))

(add-hook 'python-mode-hook #'lsp-deferred)
(setq dap-python-debugger 'debugpy)

;; pip install importmagic
(use-package importmagic
  :ensure t

  :bind
  (:map importmagic-mode-map
	("C-c C-o" . importmagic-fix-symbol-at-point))
  :hook
  (python-mode . importmagic-mode)
  
  :config
  (unbind-key "C-c C-l" importmagic-mode-map))

(provide 'init-python)

;;; init-python.el ends here
