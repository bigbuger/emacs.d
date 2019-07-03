
;;========================================================;;
;;  haskell                                               ;;
(require 'haskell-mode)
(require 'haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'linum-mode)

;;(add-to-list 'load-path "~/.emacs.d/lisp/ghc-mod-1.11.3/elisp")
;;(load "~/.emacs.d/lisp/ghc-mod-1.11.3/elisp/ghc.el")

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;(require 'flymake-haskell-multi)
;;(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)

(add-to-list 'company-backends '(company-ghc :with company-yasnippet))

(setq haskell-program-name "ghci")
;; 添加菜单项
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)


(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
(define-key haskell-mode-map (kbd "<f5>") 'haskell-process-load-file)

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (setq-local flycheck-disabled-checkers '(haskell-stack-ghc))))

;;;;;;;;;;;;;;; end haskell   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
