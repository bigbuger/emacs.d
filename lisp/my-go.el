;;; my-go.el --- golang config

;;; Commentary:
;; 

;;; Code:


;;(require 'go-autocomplete)

(require 'go-dlv)
(require 'go-guru)
(require 'go-rename)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

(require 'dap-go)

(add-hook 'go-mode-hook
	  (lambda ()
	    (setq-local company-backends '((company-go :with company-yasnippet)
					   (company-files :with company-yasnippet)
					   (company-dabbrev :with company-yasnippet)))))
(setenv "GO111MODULE" "off")
;;(require 'go-flymake)
;;(require 'go-flycheck)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook #'lsp)

;; (define-key go-mode-map
;;   (kbd "M-.") 'godef-jump)

(add-hook 'go-mode-hook (lambda ()
			  (setq-local flycheck-disabled-checkers '(go-staticcheck))))

(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;;;;;;;; end of go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-go)

;;; my-go.el ends here
