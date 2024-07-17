;;; init-go.el --- golang config

;;; Commentary:
;; 

;;; Code:



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
(require 'go-mode)
(require 'go-dlv)
(require 'go-gen-test)
(require 'go-impl)
(require 'go-tag)
(require 'go-fill-struct)
(require 'gorepl-mode)
(require 'gotest)


(require 'dap-dlv-go)

;; (require 'go-ts-mode)
;; (add-to-list 'major-mode-remap-alist
;;              '(go-mode . go-ts-mode))
;; (add-to-list 'major-mode-remap-alist
;;              '(go-dot-mod . go-mod-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-dot-mod-mode))

(setq go-fontify-function-calls nil)

;;(setenv "GO111MODULE" "off")
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(with-eval-after-load 'lsp-go
  (setf (lsp--client-completion-in-comments? (gethash 'gopls lsp-clients)) nil))
(setq lsp-go-use-placeholders nil)

(lsp-register-custom-settings
 '(("go.linitTool" "golangci-lint" nil)
   ("gopls.analyses.simplifycompositelit" t t)
   ("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)
   ("gopls.vulncheck" "Imports" nil)
   ;; ("gopls.matcher" "CaseInsensitive")
   ("gopls.analyses.ST1001" nil t) ;; dot import warning
   
   ("gopls.hints.assignVariableTypes" nil t)
   ("gopls.hints.compositeLiteralFields" nil t)
   ("gopls.hints.compositeLiteralTypes" nil t)
   ("gopls.hints.constantValues" t t)
   ("gopls.hints.functionTypeParameters" nil t)
   ("gopls.hints.parameterNames" t t)
   ("gopls.hints.rangeVariableTypes" nil t)))

(defcustom enable-golangci-lint nil
  "Use golangci lint in flycheck or not."
  :group 'lsp-go
  :type 'boolean)
(add-to-list 'load-path "~/.emacs.d/lisp/libs/flycheck-golangci-lint")
(require 'flycheck-golangci-lint)
(setq flycheck-golangci-lint-config "~/.golangci.yml")

(when enable-golangci-lint
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'go-mode)
		(setq flycheck-local-checkers '((lsp . ((next-checkers . ((warning . golangci-lint)))))))))))

(add-hook 'go-mode-hook
	  (lambda ()
	    ;; (setq-local lsp-diagnostics-provider :none)
	    (setq-local flycheck-disabled-checkers '(go-gofmt
						     go-golint
						     go-vet
						     ;; go-build
						     ;; go-test
						     go-errcheck
						     go-staticcheck
						     go-unconvert)
			go-test-args "-v -count=1")
	    (setq-local lsp-inlay-hint-enable t)
            ;; (when (flycheck-may-use-checker 'go-build)
	    ;;   (flycheck-select-checker 'go-build))
	    (lsp-deferred)))

(add-hook 'go-dot-mod-mode-hook
	  (lambda ()
	    (display-line-numbers-mode t)
	    (lsp-deferred)))
(add-hook 'go-dot-work-mode-hook
	  (lambda ()
	    (display-line-numbers-mode t)
	    (lsp-deferred)))

;; (with-eval-after-load 'projectile
;;   (add-to-list 'projectile-project-root-files "go.mod")
;;   (add-to-list 'projectile-project-root-files-bottom-up "go.mod"))

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(gopanic . ("^\t\\([[:alnum:]-_/@\\\\.]+.go\\):\\([0-9]+\\)" 1 2))
	       '(gocompile . ("^\\([[:alnum:]-_/@\\\\.]+.go\\):\\([0-9]+\\)")))
  (add-to-list 'compilation-error-regexp-alist 'gopanic))

(setq gofmt-show-errors nil)
;; gof is a shell file
;; #+begin_src shell
;; #!/bin/zsh
;; gofmt -s $@ && goimports $@
;; #+end_src
(setq gofmt-command "gof")
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;;(add-hook 'before-save-hook 'gofmt-before-save)



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

(add-hook 'go-mode-hook
	  (lambda ()
	    (when (functionp 'consult-dash)
	      (setq-local consult-dash-docsets
			  (append '("Go") consult-dash-docsets)))))


(defun smart-go-run (orig-fun &rest args)
  (if (derived-mode-p 'go-mode)
    (let ((default-directory (consult--go-root)))
      (apply orig-fun args))
    (apply orig-fun args)))
(advice-add 'smart-compile :around #'smart-go-run)

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


(add-hook 'go-mode-hook
	  (lambda ()
	    (setq rmsbolt-default-directory
			   (expand-file-name (string-replace "\n" "" (shell-command-to-string "dirname $(go env GOMOD)"))))
	    (setq-local rmsbolt-languages
			`((go-mode
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

(require 'company-go-tag)
(add-hook 'go-mode-hook
          (lambda ()
	    (setq-local company-backends
			(cl-adjoin 'company-go-tag company-backends :test #'equal))))

(defconst go-time-format-alist
  '(("yyyy" . "2006")	 ;; long year
    ("yy" . "06")	 ;; year
    ("MM" . "01") ;; zero month
    ("dd" . "02") ;; zero day
    ("HH" . "15") ;; hour
    ("hh" . "03") ;; zero hour 12
    ("mm" . "04") ;; zero minute
    ("ss" . "05") ;; zero second
    ("a" . "PM") ;; pm
    ))

(defun convert-to-go-time-format (str &optional not-insert)
  "Convert `STR' to go time format."
  (interactive (list (read-string "Format: " "yyyy-MM-dd HH:mm:ss")))
  (let ((result str))
    (dolist (r go-time-format-alist)
      (setq result (string-replace (car r) (cdr r) result)))
    (unless not-insert
      (insert result))
    result))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "s-g t") #'go-tag-add)
  (define-key go-mode-map (kbd "s-g T") #'go-tag-remove)
  (define-key go-mode-map (kbd "s-g i") #'go-impl)
  (define-key go-mode-map (kbd "s-g f") #'gofmt)
  (define-key go-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-dot-mod-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-dot-work-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-mode-map (kbd "s-g r") #'go-run)
  (define-key go-mode-map (kbd "s-g s-t") #'convert-to-go-time-format))


(provide 'init-go)

;;; init-go.el ends here
