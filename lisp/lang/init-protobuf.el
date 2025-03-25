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

(flycheck-define-checker protobuf-protoc
  "A protobuf syntax checker using the protoc compiler.

See URL `https://developers.google.com/protocol-buffers/'."
  :command ("protoc" "--error_format" "gcc"
            (eval (concat "--java_out=" (flycheck-temp-dir-system)))
            ;; Add the current directory to resolve imports
            (eval (concat "--proto_path="
                          (file-name-directory (buffer-file-name))))
            ;; Add other import paths; this needs to be after the current
            ;; directory to produce the right output.  See URL
            ;; `https://github.com/flycheck/flycheck/pull/1655'
            (option-list "--proto_path=" flycheck-protoc-import-path concat)
            source-inplace)
  :error-patterns
  ((info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
         ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (message) line-end)
   (error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":" line-end))
  :modes protobuf-mode
  :predicate (lambda () (buffer-file-name)))

(use-package pcmpl-args
  :init
  (defalias 'pcomplete/protoc 'pcmpl-args-pcomplete-on-help))


(provide 'init-protobuf)

;;; init-protobuf.el ends here
