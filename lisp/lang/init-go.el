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

(require 'go-ts-mode)

(setq treesit-language-source-alist
      (append '((go "https://github.com/tree-sitter/tree-sitter-go")
		(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
	      treesit-language-source-alist))

(add-to-list 'major-mode-remap-alist
             '(go-mode . go-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(go-dot-mod . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
;;(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-dot-mod-mode))

(setq go-fontify-function-calls nil)

;;(setenv "GO111MODULE" "off")
(add-hook 'go-ts-mode-hook 'flycheck-mode)
;;(add-hook 'go-ts-mode-hook 'go-eldoc-setup)

(with-eval-after-load 'lsp-go
  (setf (lsp--client-completion-in-comments? (gethash 'gopls lsp-clients)) nil))
;; (setq lsp-go-use-placeholders nil)

(lsp-register-custom-settings
 '(("gopls.analyses.simplifycompositelit" t t)
   ("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)
   ("gopls.vulncheck" "Imports" nil)
   ;; ("gopls.matcher" "CaseInsensitive")
   ;; ("gopls.completeFunctionCalls" nil t)
   ("gopls.analyses.ST1001" nil t) ;; dot import warning

   ("gopls.hints.assignVariableTypes" nil t)
   ("gopls.hints.compositeLiteralFields" t t)
   ("gopls.hints.compositeLiteralTypes" nil t)
   ("gopls.hints.constantValues" t t)
   ("gopls.hints.functionTypeParameters" nil t)
   ("gopls.hints.parameterNames" t t)
   ("gopls.hints.rangeVariableTypes" nil t)))

(setq lsp-golangci-lint-fast t)
(setq lsp-golangci-lint-enable '("makezero"))
(defun my-lsp-golangci-lint--get-initialization-options ()
  "Return initialization options for golangci-lint-langserver."
  (let ((opts (make-hash-table :test 'equal))
        (command (vconcat `(,lsp-golangci-lint-path)
                          ["run" "--out-format=json" "--issues-exit-code=0"]
                          (lsp-golangci-lint--run-args))))
    (puthash "command" command opts)
    opts))
(advice-add 'lsp-golangci-lint--get-initialization-options :override 'my-lsp-golangci-lint--get-initialization-options)
;; (add-to-list 'lsp-disabled-clients 'golangci-lint) ;; too slow

(setq go-ts-mode-indent-offset 4)
(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (setq-local defun-prompt-regexp go-func-regexp
			tab-width 4
			flycheck-disabled-checkers '(go-gofmt
						     go-golint
						     go-vet
						     ;; go-build
						     ;; go-test
						     go-errcheck
						     go-staticcheck
						     go-unconvert)
			go-test-args "-v -count=1 -gcflags=all=-l"
			lsp-inlay-hint-enable t)
            
	    (lsp-deferred)))

(add-hook 'go-mod-ts-mode-hook
	  (lambda ()
	    (setq-local tab-width 4)
	    (display-line-numbers-mode t)
	    (lsp-deferred)))
(add-hook 'go-dot-work-mode-hook
	  (lambda ()
	    (setq-local tab-width 4)
	    (display-line-numbers-mode t)
	    (lsp-deferred)))

;; (with-eval-after-load 'projectile
;;   (add-to-list 'projectile-project-root-files "go.mod"))

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



(defvar golangci-lint-cmd "LOG_LEVEL=error golangci-lint run --issues-exit-code 0 --out-format=line-number --print-issued-lines=false ./..."
  "Command for `consult-golangci-lint'.")

(defun consult--go-root ()
  "Find go mod root."
  (expand-file-name (string-replace "\n" "" (shell-command-to-string "dirname $(go env GOMOD)"))))

(defmacro with-go-project-root (body)
  `(let ((default-directory (or (consult--go-root)
				default-directory)))
     ,body))

(defun golangci-lint ()
  "Run golangci-lint, and diplay the result by `grep-mode'."
  (interactive)
  (with-go-project-root
   (let ((cmd (read-shell-command
	       "Run golangci-lint like this: "
	       golangci-lint-cmd)))
     (grep cmd))))

(defun go-mod-tidy ()
  "Run `go mod tidy by compile'."
  (interactive)
  (with-go-project-root
   (compile "go mod tidy")))

(defvar go-get-history nil)
(defun go-get (pkg)
  "Run `go get PKG' by compile."
  (interactive
   (let ((pkgs (mapcar (lambda (s) (car (split-string s )))
		       (process-lines "go" "list" "-m" "all"))))
     (list (completing-read "go get: "
			    pkgs
			    nil
			    nil
			    nil
			    'go-get-history))))
  (with-go-project-root
   (compile (concat "go get " pkg))))


;; (with-eval-after-load 'projectile
;;   (projectile-register-project-type 'go-mod '("go.mod")
;; 				    :compile "go build ."
;; 				    :test "go test ./..."
;; 				    :run "go run ."
;; 				    :test-suffix "_test.go"))

(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (when (functionp 'consult-dash)
	      (setq-local consult-dash-docsets
			  (append '("Go") consult-dash-docsets)))))


(defun smart-go-run (orig-fun &rest args)
  (if (derived-mode-p 'go-mode)
      (with-go-project-root
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


(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (ignore-errors
	      (setq-local rmsbolt-default-directory
			  (projectile-acquire-root))
	      (setq-local rmsbolt-languages
			  `((go-ts-mode
			     . ,(make-rmsbolt-lang :compile-cmd "go"
						   :supports-asm t
						   :supports-disass t
						   :objdumper nil
						   :compile-cmd-function #'rmsbolt--go-plan9-compile-cmd
						   :process-asm-custom-fn #'rmsbolt--process-go-plan9-lines)))))))


;; go install github.com/godoctor/godoctor@latest
;; (use-package godoctor)
;; overwrite godoctor--get-pos-region for utf-8 charset
;; (defun godoctor--get-pos-region ()
;;   (let* ((start (position-bytes (region-beginning)))
;; 	 (end (position-bytes (region-end)))
;;          (len (- end start)))
;;     (format "%d,%d" start len)))

(setq go-tag-args '("-transform" "camelcase"))
(require 'company-go-tag)
(add-hook 'go-ts-mode-hook
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

(with-eval-after-load 'go-ts-mode
  (define-key go-ts-mode-map (kbd "C-c C-a") #'go-import-add)
  
  (define-key go-ts-mode-map (kbd "s-g t") #'go-tag-add)
  (define-key go-ts-mode-map (kbd "s-g T") #'go-tag-remove)
  (define-key go-ts-mode-map (kbd "s-g i") #'go-impl)
  (define-key go-ts-mode-map (kbd "s-g f") #'gofmt)
  (define-key go-ts-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-mod-ts-mode-map (kbd "s-g l") #'golangci-lint)
  (define-key go-dot-work-mode-map (kbd "s-g l") #'golangci-lint)

  (define-key go-ts-mode-map (kbd "s-g m t") #'go-mod-tidy)
  (define-key go-mod-ts-mode-map (kbd "s-g m t") #'go-mod-tidy)
  (define-key go-dot-work-mode-map (kbd "s-g m t") #'go-mod-tidy)

  (define-key go-ts-mode-map (kbd "s-g g") #'go-get)
  (define-key go-mod-ts-mode-map (kbd "s-g g") #'go-get)
  (define-key go-dot-work-mode-map (kbd "s-g g") #'go-get)
  
  
  (define-key go-ts-mode-map (kbd "s-g r") #'go-run)
  (define-key go-ts-mode-map (kbd "s-g s-t") #'convert-to-go-time-format))

(defconst pcmpl-go-commands
  '("build" "clean" "doc" "env" "fix" "fmt"
    "generate" "get" "install" "list" "mod" "work"
    "run" "test" "tool" "version" "vet")
  "List of `go' commands.")

;; go help build | grep "^\s*-" | awk '{print "\""$1"\""}' | uniq
(defconst pcml-go-build-flag
  '("-C" "-a" "-n" "-p"
    "-race" "-msan" "-asan"
    "-cover" "-covermode" "-coverpkg"
    "-v" "-work" "-x" "-asmflags"
    "-buildmode" "-buildvcs" "-compiler"
    "-gccgoflags" "-gcflags" "-installsuffix"
    "-ldflags" "-linkshared" "-mod"
    "-modcacherw" "-modfile" "-overlay"
    "-pgo" "-pkgdir" "-tags"
    "-trimpath" "-toolexec")
  "List of `go build' flag.")

(defun pcomplete/go ()
  "Completion for `go'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-go-commands)

  (let ((option-p (string-prefix-p "-" (symbol-name (symbol-at-point)))))
    (cond
     ((pcomplete-match "mod" 1)
      (pcomplete-here* '("download" "edit" "graph" "init" "tidy" "vendor" "verify" "why")))
     ((pcomplete-match "work" 1)
      (pcomplete-here* '("edit" "init" "vendor" "use" "sync")))
     ((pcomplete-match "build" 1)
      (while (pcomplete-here
	      (completion-table-merge
	       (when option-p
		 `("-o" ,@pcml-go-build-flag))
	       (pcomplete-entries)))))
     ((pcomplete-match "get" 1)
      (while (pcomplete-here
	      (completion-table-merge
	       (when option-p
		 `("-t" "-u" "-v" ,@pcml-go-build-flag))
	       (pcomplete-entries)))))
     ((pcomplete-match "install" 1)
      (while (pcomplete-here
	      (completion-table-merge
	       (when option-p
		 pcml-go-build-flag)
	       (pcomplete-entries)))))
     ((pcomplete-match "run" 1)
      (while (pcomplete-here
	      (completion-table-merge
	       (when option-p
		 pcml-go-build-flag)
	       (pcomplete-entries)))))
     ((pcomplete-match "test" 1)
      (while (pcomplete-here
	      (completion-table-merge
	       (when option-p
		 `("-args" "-c" "-exec" "-json" "-o"
		   "-benchtime"  "-cpu" "-list"
		   "-parallel" "-run" "-short"
		   "-timeout" "-failfast"
		   ,@pcml-go-build-flag))
	       (pcomplete-entries)))))
     ((pcomplete-match "clean" 1)
      (while (pcomplete-here
	      (completion-table-merge
	       (when option-p
		 `("-i" "-n" "-r" "-x" "-cache" "-testcache"
		   "-modcache" "-fuzzcache"
		   ,@pcml-go-build-flag))
	       (pcomplete-entries))))))))

(defun pcomplete/golangci-lint ()
  "Completion for `golangci-lint'."
  (pcomplete-here* '("cache" "completion" "config" "custom" "help" "linters" "run" "version"))
  (when (pcomplete-match "run" 1)
    (pcomplete-here-using-help "golangci-lint run -h")))


(use-package ob-go
  :demand t
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(go . t))))

(use-package topsy
  :init
  (defun topsy--go-beginning-scope ()
    "Return the line moved to top ast parent."
    (when (> (window-start) 1)
      (save-excursion
	(goto-char (window-start))
	(let ((node (treesit-parent-until
		     (treesit-node-at (point))
		     (lambda (p)
		       (string-equal "source_file"
				     (treesit-node-type (treesit-node-parent p)))))))
	  (goto-char (treesit-node-start node))
	  (font-lock-ensure (point) (pos-eol))
	  (buffer-substring (point) (pos-eol))))))
  
  :config
  (setf (alist-get 'go-ts-mode  topsy-mode-functions) #'topsy--go-beginning-scope)
  (add-hook 'topsy-mode-hook
	    (lambda ()
	      (setq-local lsp-headerline-breadcrumb-enable nil)))
  
  :hook
  (go-ts-mode . topsy-mode))

(provide 'init-go)

;;; init-go.el ends here
