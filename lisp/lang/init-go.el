;;; init-go.el --- golang config

;;; Commentary:
;; 

;;; Code:

(require 'go-ts-mode)

;; go install github.com/go-delve/delve/cmd/dlv
;; go get golang.org/x/tools/cmd/guru
;; go get golang.org/x/tools/cmd/gorename
;; go install github.com/golangci/golangci-lint/cmd/golangci-lint@
;; go get golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install github.com/cweill/gotests/...@latest
;; go install github.com/josharian/impl@latest
;; go install github.com/fatih/gomodifytags@latest
;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
;; go install github.com/x-motemen/gore/cmd/gore@latest
(require 'go-dlv)
(require 'go-gen-test)
(require 'go-impl)
(require 'go-tag)
(require 'go-fill-struct)
(require 'gorepl-mode)
(require 'gotest)


(require 'dap-dlv-go)

(add-to-list 'major-mode-remap-alist
             '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(go-dot-mod . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))


(setq go-test-args "-v")

(setq go-fontify-function-calls nil)

;;(setenv "GO111MODULE" "off")
(add-hook 'go-ts-mode-hook 'flycheck-mode)
(add-hook 'go-ts-mode-hook 'go-eldoc-setup)


(lsp-register-custom-settings
 '(("go.linitTool" "golangci-lint" nil)
   ("gopls.analyses.simplifycompositelit" t t)
   ("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)
   
   ("gopls.hints.assignVariableTypes" nil t)
   ("gopls.hints.compositeLiteralFields" nil t)
   ("gopls.hints.compositeLiteralTypes" nil t)
   ("gopls.hints.constantValues" t t)
   ("gopls.hints.functionTypeParameters" nil t)
   ("gopls.hints.parameterNames" nil t)
   ("gopls.hints.rangeVariableTypes" nil t)))

(setq go-test-args "-v -count=1")

;; (add-to-list 'load-path "~/.emacs.d/lisp/libs/flycheck-golangci-lint")
;; (require 'flycheck-golangci-lint)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'go-mode)
;;               (setq flycheck-local-checkers '((lsp . ((next-checkers . ((warning . golangci-lint))))))))))

(add-hook 'go-ts-mode-hook
	  (lambda ()
	    ;; (setq-local lsp-diagnostics-provider :none)
	    (setq-local flycheck-disabled-checkers
			'(go-gofmt
			  go-golint
			  go-vet
			  ;; go-build
			  ;; go-test
			  go-errcheck
			  go-staticcheck
			  go-unconvert))
            ;; (when (flycheck-may-use-checker 'go-build)
	    ;;   (flycheck-select-checker 'go-build))
	    (lsp-deferred)))

(add-hook 'go-ts-dot-mod-mode-hook
	  (lambda ()
	    (linum-mode)
	    (lsp-deferred)))
(add-hook 'go-dot-work-mode-hook
	  (lambda ()
	    (linum-mode)))


(setq gofmt-command "goimports")
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(defvar golangci-lint-cmd "LOG_LEVEL=error golangci-lint run --print-issued-lines=false --out-format=line-number ./..."
  "Command for `consult-golangci-lint'.")

(defun consult--go-root ()
  "Find go mod root."
  (expand-file-name (string-replace "\n" "" (shell-command-to-string "dirname $(go env GOMOD)"))))

(defun golangci-lint ()
  "Run golangci-lint, and diplay the result by `grep-mode'."
  (interactive)
  (let ((default-directory (or (consult--go-root)
			       default-directory))
	(cmd (read-shell-command
	      "Run golangci-lint like this: "
	      golangci-lint-cmd)))
    (grep cmd)))


(with-eval-after-load 'projectile
  (projectile-register-project-type 'go-mod '("go.mod")
				    :compile "go build ."
				    :test "go test ./..."
				    :run "go run ."
				    :test-suffix "_test.go"))

(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "s-g t") #'go-tag-add)
  (define-key go-ts-mode-map (kbd "s-g T") #'go-tag-remove)
  (define-key go-ts-mode-map (kbd "s-g i") #'go-impl)
  (define-key go-ts-mode-map (kbd "s-g f") #'gofmt)
  (define-key go-ts-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-mod-ts-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-dot-work-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-ts-mode-map (kbd "s-g r") #'go-run))

(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (when (functionp 'consult-dash)
	      (setq-local consult-dash-docsets
			  (append '("Go") consult-dash-docsets)))))


(require 'rmsbolt)

(cl-defun rmsbolt--go-plan9-compile-cmd (&key src-buffer)
  "Process a compile command for go."
  (rmsbolt--with-files
   src-buffer
   (let* ((cmd (buffer-local-value 'rmsbolt-command src-buffer))
          (cmd (mapconcat #'identity
                          (list cmd
				"build"
				"-gcflags=-S"
                                "&>" output-filename
                                src-filename)
                          " ")))
     cmd)))

(cl-defun rmsbolt--process-go-plan9-lines (_src-buffer asm-lines)
  (let ((source-linum nil)
        (result nil))
    (dolist (line asm-lines)
      (if (not
	   (string-match (rx bol "\t"
			     (group "0x" (1+ hex))
			     (1+ " ")
			     (group (1+ digit))
			     (1+ " ")
			     "("
			     (group (0+ (not ":"))) ":"
			     (group (1+ digit))
			     ")"
			     (* any) eol)
			 line))
	  ;; just push the var with no linum
	  (push line result)
	;; Grab line numbers
	(unless (string-empty-p (match-string 4 line))
          (setq source-linum
		(string-to-number (match-string 4 line))))
	(when source-linum
          (add-text-properties 0 (length line)
                               `(rmsbolt-src-line ,source-linum) line))
	;; Add line
	(push line result)))
    (nreverse result)))


(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (setq rmsbolt-default-directory
			   (expand-file-name (string-replace "\n" "" (shell-command-to-string "dirname $(go env GOMOD)"))))
	    (setq-local rmsbolt-languages
			`((go-ts-mode
			   . ,(make-rmsbolt-lang :compile-cmd "go"
						 :supports-asm t
						 :supports-disass t
						 :objdumper nil
						 :compile-cmd-function #'rmsbolt--go-plan9-compile-cmd
						 :process-asm-custom-fn #'rmsbolt--process-go-plan9-lines))))))


;; go install github.com/godoctor/godoctor@latest

;; (require 'godoctor)
;; overwrite godoctor--get-pos-region for utf-8 charset
;; (defun godoctor--get-pos-region ()
;;   (let* ((start (position-bytes (region-beginning)))
;; 	 (end (position-bytes (region-end)))
;;          (len (- end start)))
;;     (format "%d,%d" start len)))

(provide 'init-go)

;;; init-go.el ends here
