;;; init-protobuf.el --- protobuf setting

;;; Commentary:
;; 


;;; Code:

(require 'protobuf-mode)


(require 'lsp-mode)

(add-hook 'protobuf-mode-hook
	  (lambda ()
	    (setq-local lsp-diagnostics-provider :none)
	    (lsp-deferred)))

(add-to-list 'load-path "~/.emacs.d/lisp/libs/company-proto")
(require 'company-proto)
(add-hook 'protobuf-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-proto :with company-yasnippet) company-backends :test #'equal))))

(setq lsp-buf-args '("beta" "lsp" "--timeout" "0"))
(use-package ob-grpc
  :load-path "~/.emacs.d/lisp/libs/ob-grpc"
  :bind (:map org-mode-map
              ("C-c g i" . ob-grpc-init)
              ("C-c g b" . ob-grpc-insert-block)))

(provide 'init-protobuf)

;;; init-protobuf.el ends here
