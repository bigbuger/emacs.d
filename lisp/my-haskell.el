;;; my-haskell.el --- haskell config

;;; Commentary:
;; 

;;; Code:

(require 'haskell-mode)
(require 'haskell-indent)
;;(require 'haskell-interactive-mode)
;;(require 'haskell-process)
(require 'dante)

;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(with-eval-after-load 'highlight-indent-guides
  (add-hook 'haskell-mode-hook (lambda ()
				 (highlight-indent-guides-mode -1))))

;;(autoload 'ghc-init "ghc" nil t)
;;(autoload 'ghc-debug "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;(require 'flymake-haskell-multi)
;;(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

(add-to-list 'company-backends '(company-ghc :with company-yasnippet))

(setq haskell-program-name "ghci")
;; 添加菜单项
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)



(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
(define-key haskell-mode-map (kbd "<f5>") 'haskell-process-load-file)

;; (add-hook 'haskell-mode-hook
;; 	  (lambda ()
;; 	   (setq-local flycheck-disabled-checkers '(haskell-stack-ghc))))

(add-hook 'haskell-mode-hook 'dante-mode)
(flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))

(provide 'my-haskell)

;;; my-haskell.el ends here
