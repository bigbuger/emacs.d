;;; init-lsp.el --- lsp 配置，使用 lsp-mode

;;; Commentary:
;; 
;;; Code:

(require 'lsp-mode)
(require 'lsp-ui)
(require 'dap-mode)

(setq lsp-warn-no-matched-clients nil)
(setq lsp-auto-guess-root t)
(setq lsp-prefer-flymake :none)
(setq lsp-ui-flycheck-enable t)
;; (setq lsp-inlay-hint-enable t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-headerline-breadcrumb-enable t)
;; (add-hook 'lsp-mode-hook
;; 	  '(lambda ()
;; 	     (setq-local header-line-format
;; 			 '((t
;; 			    (:eval
;; 			     (frame-parameter nil 'lsp-headerline--string)))))))

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-show-with-cursor nil)

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions t)

(setq lsp-ui-doc-include-signature t)

(setq lsp-signature-auto-activate t)
(setq lsp-signature-doc-lines 1)
(setq lsp-signature-render-documentation nil)
;;(setq lsp-signature-function 'lsp-signature-posframe)
(define-key lsp-mode-map (kbd "s-d") 'lsp-signature-activate)
(define-key lsp-mode-map (kbd "s-l l") 'lsp-ui-sideline-mode)

(setq lsp-auto-execute-action nil)
(setq lsp-modeline-code-actions-segments '(count icon))

(setq lsp-completion-provider :none)
;; (add-hook 'lsp-completion-mode-hook
;; 	  #'(lambda ()
;; 	     (setq-local company-backends
;; 			 (cl-adjoin '(company-capf :separate company-yasnippet)
;; 				    company-backends :test #'equal))))

(setq dap-auto-configure-features '(locals controls tooltip))
;; (define-key lsp-mode-map (kbd "M-?") 'lsp-ui-peek-find-references)
(define-key lsp-mode-map [f5] 'dap-debug)
(define-key lsp-mode-map (kbd "C-<f5>") 'dap-hydra)
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'semgrep-ls))

(provide 'init-3-lsp)

;;; init-3-lsp.el ends here
