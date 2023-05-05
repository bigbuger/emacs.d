;;; my-haskell.el --- haskell config

;;; Commentary:
;; 

;;; Code:

(require 'haskell-mode)
(require 'haskell-indent)
;; (require 'dante)
(require 'lsp)
(require 'lsp-haskell)

;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp-deferred)
(add-hook 'haskell-literate-mode-hook #'lsp-deferred)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(with-eval-after-load 'highlight-indent-guides
  (add-hook 'haskell-mode-hook (lambda ()
				 (highlight-indent-guides-mode -1))))


(add-hook 'haskell-interactive-mode-hook 'company-mode)
(add-hook 'haskell-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-ghc company-ghci :with company-yasnippet) company-backends :test #'equal))))

(setq haskell-program-name "ghci")


(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
(define-key haskell-mode-map (kbd "<f5>") 'haskell-process-load-file)


;; (add-hook 'haskell-mode-hook 'dante-mode)
;; (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))

(setq haskell-font-lock-symbols 1)

(require 'smartparens)
(sp-local-pair 'haskell-mode "(" nil :actions '(:rem insert))

(provide 'my-haskell)

;;; my-haskell.el ends here
