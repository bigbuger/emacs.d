;;; my-protobuf.el --- protobuf setting

;;; Commentary:
;; 


;;; Code:

(require 'protobuf-mode)


(require 'lsp-mode)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(protobuf-mode . "\\.proto\\'"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("bufls" "serve"))
                     :activation-fn (lsp-activate-on "\\.proto\\'")
                     :server-id 'bufls)))

(add-hook 'protobuf-mode-hook
	  (lambda ()
	    (setq-local lsp-diagnostics-provider :none)
	    (lsp-deferred)))


(provide 'my-protobuf)

;;; my-protobuf.el ends here
