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

;; copy from https://github.com/shsms/ob-grpc/blob/main/ob-grpc.el
(defun ob-grpc--concat-imports (import-paths)
  "Take a list of IMPORT-PATHS and return string of grpcurl cli arguments."
  (let (result)
    (dolist (item import-paths result)
      (setq result (concat " -import-path " item result)))
    result))
(defun ob-grpc--grpcurl-describe (proto-file import-paths name &optional msg-template)
  "Return a `grpcurl describe' for NAME, defined in PROTO-FILE and IMPORT-PATHS."
  (let ((command (message "grpcurl %s %s -proto %s describe %s"
			  (or (ob-grpc--concat-imports import-paths) "")
			  (if msg-template " --msg-template " "")
			  proto-file
			  name)))
    (message (shell-command-to-string command))))

(defun ob-grpc--grpcurl-msg-template ()
  "Return the method signature and request msg template for method NAME, using grpcurl."
  (let* ((args (caddr (org-babel-get-src-block-info)))
	 (method (cdr (assq :method args)))
	 (proto-file (cdr (assq :proto args)))
	 (import-paths (cdr (assq :import-paths args)))
	 (method-description (ob-grpc--grpcurl-describe proto-file import-paths method))
	 (msg-template "uninitialized")
         (block-prefix nil))
    (message "%s" method-description)
    (save-match-data
      (and (string-match
	    "^rpc \\([A-Za-z_0-9]+\\) (\\( stream\\)? \\([a-zA-Z._0-9]+\\) ) returns (\\( stream\\)? \\([a-zA-Z._0-9]+\\) )"
	    method-description)
           (let ((decl (match-string 0 method-description))
                 (method-short (match-string 1 method-description))
		 (req-stream (match-string 2 method-description))
		 (req-message (match-string 3 method-description))
		 (resp-stream (match-string 4 method-description))
		 (resp-message (match-string 5 method-description))
                 )
             (let ((msg-desc (ob-grpc--grpcurl-describe proto-file import-paths req-message t)))
	       (and (string-match "Message template:\n\\(.*\\(?:\n.*\\)*?\\)\\(?:\n\\'\\)"
				  msg-desc)
                    (setq msg-template (match-string 1 msg-desc)))))))
    msg-template))

(defun org-grpc-insert-request-template ()
  (interactive)
  (insert (ob-grpc--grpcurl-msg-template)))

(add-to-list 'org-src-lang-modes '("grpc" . json))
(defun org-babel-execute:grpc (body params)
  "Execute a block of grpc code with org-babel."
  (let* ((name (nth 4 (org-babel-get-src-block-info)))
	 (out-buffer (format "ob-grpc%s.json"  (if name (concat "-" name) "")))
	 (err-buffer "*ob-grpc-stderr*")
	 (in-file (org-babel-temp-file "grpc.json"))
	 (cmd (cdr (assq :cmd params)))
	 (proto (cdr (assq :proto params)))
	 (method (cdr (assq :method params)))
	 (service (cdr (assq :service params)))
	 (import-paths (cdr (assq :import-paths params)))
	 (args (list cmd
		     "-d @"
		     (when proto (format "-proto %s" proto))
		     (when import-paths (ob-grpc--concat-imports import-paths))
		     (format "%s %s" service method)))
	 (grpcurl (format "cat %s | grpcurl %s | jq '.'" in-file (string-join args " "))))
    (with-temp-file in-file
      (insert body))
    (message "ob-grpc: %s" grpcurl)
    
    (async-shell-command grpcurl out-buffer)
    ))

(provide 'ob-grpc)

(defun protobuf-quick-test ()
  (interactive)
  (let ((file (file-relative-name (buffer-file-name)))
	method package service)
    (save-excursion
      (beginning-of-line)
      (when (search-forward-regexp "^\\s-*rpc\\s-*\\(\\sw+\\)\\s-*(\\s-*\\sw+\\s-*)\\s-*returns" (pos-eol) t)
	(setq method (match-string 1)))
      (when (search-backward-regexp "^\\s-*service\\s-*\\(\\sw+\\)\\s-*{" (point-min) t 1)
	(setq service (match-string 1)))
      (when (search-backward-regexp "^\\s-*package\\s-*\\(.*?\\)\\s-*;" (point-min) t 1)
	(setq package (match-string 1))))
    (when method
      (let ((buffer (get-buffer-create (format "* Grpc test %s/%s *" service method))))
	(with-current-buffer buffer
	  (org-mode)
	  (when (= (point-min) (point-max))
	    (insert (format "#+NAME: %s
#+HEADER: :method %s%s.%s
#+HEADER: :service \"127.0.0.1:30000\"
#+HEADER: :proto \"%s\"
#+HEADER: :import-paths '(\".\")
"
			    method
			    (if package (concat package ".") "") service method
			    file))
	    (insert "#+begin_src grpc :results none :cmd \"-plaintext\" \n")
	    (insert "\n\n")
	    (insert "#+end_src")
	    (forward-line -2)
	    (org-grpc-insert-request-template)))
	(display-buffer buffer)))))

(define-key protobuf-mode-map (kbd "C-c C-t") #'protobuf-quick-test)

(provide 'init-protobuf)

;;; init-protobuf.el ends here
