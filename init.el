
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(set-default-coding-systems 'utf-8)

(setq shell-file-name "/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "global.el")
(load "my-command.el")
(load "my-hideShow.el")
(load "my-autoComple.el")
(load "my-yasnippet.el")
(load "my-go.el")
(load "my-haskell.el")
(load "my-scheme.el")
(load "my-ruby.el")

(global-linum-mode 1)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh t)
(setq speedbar-show-unknown-files t)
(global-set-key [f8] 'sr-speedbar-toggle)

(global-undo-tree-mode)

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)




(when (memq window-system '(mac ns))
  (let (
  	(mypaths
  	 (append (split-string (getenv "PATH") ":")
  		 (list 
  		  "/usr/local/bin"
  		  "/usr/local/go/bin"
		  (concat (getenv "HOME") "/Library/Haskell/bin")
  		  (concat (getenv "HOME") "/go/bin")))))
    (setenv "PATH"  (mapconcat 'identity mypaths ":"))
    (setq exec-path (append mypaths (list "." exec-directory)) ))
  ;;(exec-path-from-shell-initialize)
)

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;;(global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (idris-mode flymake-haskell-multi flycheck yasnippet visual-regexp undo-tree sr-speedbar smex ruby-electric rsense go-scratch go-playground go-eldoc go-dlv go-autocomplete ghc flymake-ruby color-theme ac-inf-ruby ac-haskell-process))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
