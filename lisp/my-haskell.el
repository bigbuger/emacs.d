
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

(require 'flymake-haskell-multi)

(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
(push '(company-ghc :with company-yasnippet) company-backends)

(setq haskell-program-name "ghci")
;; 添加菜单项
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
(defun load-haskell-file ()
  (interactive)
  (progn
    (let ((b (current-buffer)))
      (run-haskell)
      (switch-to-buffer b)
      (inferior-haskell-load-file))))
(add-hook 'haskell-mode-hook
	  '(lambda ()
	     (local-set-key (kbd "<f5>") 'load-haskell-file)))

;;;;;;;;;;;;;;; end haskell   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
