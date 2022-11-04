;;; my-go.el --- golang config

;;; Commentary:
;; 

;;; Code:


;;(require 'go-autocomplete)


;; go install github.com/go-delve/delve/cmd/dlv
;; go get golang.org/x/tools/cmd/guru
;; go get golang.org/x/tools/cmd/gorename
(require 'go-dlv)
(require 'go-guru)
(require 'go-rename)

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


;; GO111MODULE=on go get golang.org/x/tools/gopls@latest

(lsp-register-custom-settings
 '(("go.linitTool" "staticcheck" nil)
   ("gopls.analyses.simplifycompositelit" t t)
   ("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))


;;(add-hook 'go-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp-deferred)

;; (defvar-local flycheck-local-checkers nil)
;; (defun +flycheck-checker-get(fn checker property)
;;   (or (alist-get property (alist-get checker flycheck-local-checkers))
;;       (funcall fn checker property)))
;; (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'go-mode)
;;               (setq flycheck-local-checkers '((lsp . ((next-checkers . (go-staticcheck)))))))))


;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(provide 'my-go)

;;; my-go.el ends here
