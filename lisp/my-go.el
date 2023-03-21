;;; my-go.el --- golang config

;;; Commentary:
;; 

;;; Code:


;;(require 'go-autocomplete)


;; go install github.com/go-delve/delve/cmd/dlv
;; go get golang.org/x/tools/cmd/guru
;; go get golang.org/x/tools/cmd/gorename
;; go install github.com/golangci/golangci-lint/cmd/golangci-lint@
;; go get golang.org/x/tools/gopls@latest
;; go install golang.org/x/tools/cmd/goimports@latest
;; go install github.com/cweill/gotests/...@latest
;; go install github.com/josharian/impl@latest
;; go install github.com/godoctor/godoctor@latest
;; go install github.com/fatih/gomodifytags@latest
;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
;; go install github.com/x-motemen/gore/cmd/gore@latest
(require 'go-dlv)
(require 'go-guru)
(require 'go-rename)
(require 'go-gen-test)
(require 'go-impl)
(require 'godoctor)
(require 'go-tag)
(require 'go-fill-struct)
(require 'gorepl-mode)


;;(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
;;(require 'golint)

(require 'dap-dlv-go)

(setq go-test-args "-v")

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-local company-backends '((company-go :with company-yasnippet)
					   (company-files :with company-yasnippet)
					   (company-dabbrev :with company-yasnippet)))))
;;(setenv "GO111MODULE" "off")
;;(require 'go-flymake)
;;(require 'go-flycheck)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; (define-key go-mode-map
;;   (kbd "M-.") 'godef-jump)

;; (add-hook 'go-mode-hook (lambda ()
;; 			  (setq-local flycheck-disabled-checkers '(go-staticcheck))))



(defun my-go-impl ()
    (interactive)
    (let ((completing-read-function'completing-read-default))
      (call-interactively 'go-impl)))

(lsp-register-custom-settings
 '(("go.linitTool" "staticcheck" nil)
   ("gopls.analyses.simplifycompositelit" t t)
   ("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

(setq go-test-args "-v -count=1")

(add-hook 'go-mode-hook #'lsp-deferred)


(require 'flycheck-golangci-lint)
(add-hook 'go-mode-hook
	  (lambda()
            (flycheck-golangci-lint-setup)
	    (setq-local lsp-diagnostics-provider :none))) 


(setq gofmt-command "goimports")
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(add-to-list 'load-path "~/.emacs.d/lisp/counsel-golangci-lint")
(require 'counsel-golangci-lint)


(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "s-g t") #'go-tag-add)
  (define-key go-mode-map (kbd "s-g T") #'go-tag-remove)
  (define-key go-mode-map (kbd "s-g i") #'my-go-impl)
  (define-key go-mode-map (kbd "s-g f") #'gofmt))

(provide 'my-go)

;;; my-go.el ends here
