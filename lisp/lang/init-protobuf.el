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
          (lambda ()
	    (setq-local company-backends
			(cl-adjoin '(company-proto :with company-yasnippet) company-backends :test #'equal)
			lsp-diagnostics-flycheck-default-level 'warning)
	    (setq-local lsp-enable-imenu nil)		
	    (setq-local imenu-generic-expression
			'(("Message" "^[[:space:]]*message[[:space:]]+\\([[:alnum:]]+\\)" 1)
			  ("Enum" "^[[:space:]]*enum[[:space:]]+\\([[:alnum:]]+\\)" 1)
			  ("Service" "^[[:space:]]*service[[:space:]]+\\([[:alnum:]]+\\)" 1)
			  ("Function" "^[[:space:]]*rpc[[:space:]]+\\([[:alnum:]]+\\)" 1)))))

(setq lsp-buf-args '("lsp" "serve" "--timeout" "0"))

;; cargo install protols
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   '("protols"))
                  :activation-fn (lsp-activate-on "protobuf")
                  :language-id "protobuf"
                  :priority 0
                  :server-id 'protols))

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

(with-eval-after-load 'consult-imenu
  (add-to-list 'consult-imenu-config
               '(protobuf-mode
                 :types ((?m "Message" font-lock-type-face)
                         (?s "Service" font-lock-type-face)
			 (?f "Function" font-lock-type-face)))))

(add-to-list 'org-src-lang-modes '("grpc" . json))
(defun org-babel-execute:grpc (body params)
  "Execute a block of grpc code with org-babel."
  (let* ((out-buffer "ob-grpc.json")
	 (err-buffer "*ob-grpc-stderr*")
	 (in-file (org-babel-temp-file "grpc.json"))
	 (cmd (cdr (assq :cmd params)))
	 (proto (cdr (assq :args params)))
	 (method (cdr (assq :method params)))
	 (service (cdr (assq :service params)))
	 (args (list cmd
		     "-d @"
		     (when proto (format "-proto %s" proto))
		     (format "%s %s" service method)))
	 (grpcurl (format "cat %s | grpcurl %s | jq '.'" in-file (string-join args " "))))
    (with-temp-file in-file
      (insert body))
    (message "ob-grpc: %s" grpcurl)
   
    (async-shell-command grpcurl out-buffer)
    ))
(provide 'ob-grpc)

(provide 'init-protobuf)

;;; init-protobuf.el ends here
