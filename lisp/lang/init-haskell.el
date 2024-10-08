;;; init-haskell.el --- haskell config

;;; Commentary:
;; 

;;; Code:

(require 'haskell-mode)
(require 'haskell-indent)
;; (require 'dante)
(require 'lsp)
(require 'lsp-haskell)

(add-hook 'haskell-mode-hook (lambda () (setq-local lsp-ui-sideline-enable t)))

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

(setq haskell-program-name "ghci")


(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
;; (define-key haskell-mode-map (kbd "<f5>") 'haskell-process-load-file)


;; (add-hook 'haskell-mode-hook 'dante-mode)
;; (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))

(setq haskell-font-lock-symbols 1)
(add-hook 'haskell-mode-hook (lambda () (setq-local prettify-symbols-unprettify-at-point t)))

(require 'smartparens)
(sp-local-pair 'haskell-mode "(" nil :actions '(:rem insert))

(with-eval-after-load 'smart-compile
  (setf (alist-get "\\.hs\\'" smart-compile-alist nil nil #'string=) "runhaskell %f"))

(use-package consult-hoogle
  :load-path "~/.emacs.d/lisp/libs/consult-hoogle")

(use-package ghci-completion
  :init
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(haskell . t)))

(provide 'init-haskell)

;;; init-haskell.el ends here
