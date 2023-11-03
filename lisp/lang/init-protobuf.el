;;; init-protobuf.el --- protobuf setting

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

(add-to-list 'load-path "~/.emacs.d/lisp/libs/company-proto")
(require 'company-proto)
(add-hook 'protobuf-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-proto :with company-yasnippet) company-backends :test #'equal))))


(provide 'init-protobuf)

;;; init-protobuf.el ends here
