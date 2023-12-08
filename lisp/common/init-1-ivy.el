;;; init-ivy.el --- ivy 三件套，搜索神器
;;; Commentary:
;; 

;;; Code:

(require 'ivy)
(ivy-mode 1)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(setq ivy-display-style 'fancy)
(setq counsel-fzf-cmd "fd --type f --hidden --follow --exclude .git --color never '%s'")

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f1> k") 'counsel-descbinds)
(global-set-key (kbd "C-c M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c e") 'counsel-recentf)
(global-set-key (kbd "C-c s") 'counsel-rg)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-c m") 'counsel-bookmark)
(global-set-key (kbd "C-x 8") 'counsel-unicode-char)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(require 'counsel-fd)
(require 'dired-x)
(global-set-key (kbd "C-c j") 'counsel-fd-dired-jump)


(require 'ivy-rich)
(setq ivy-rich-path-style 'abbrev)
(setq ivy-rich-display-transformers-list
      (append ivy-rich-display-transformers-list
	      '(counsel-bookmark
		(:columns
		 ((ivy-rich-bookmark-type)
		  (ivy-rich-candidate (:width 10))
		  (ivy-rich-bookmark-info))))))
(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)


(require 'pyim-cregexp-utils)
(add-to-list 'ivy-preferred-re-builders
	     '(pyim-cregexp-ivy . "pyim"))

(setq ivy-re-builders-alist
      '((org-roam-node-find . pyim-cregexp-ivy)
	(counsel-find-file . pyim-cregexp-ivy)
	(counsel-recentf . pyim-cregexp-ivy)
	(t . ivy--regex-plus)))

;; ivy support projectile
(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy)
  (require 'counsel-projectile)
  (define-key projectile-command-map (kbd "s") 'counsel-projectile-rg)
  (define-key projectile-command-map (kbd "b") 'counsel-projectile-switch-to-buffer))

;; ivy support lsp

(with-eval-after-load 'lsp-mode
  (add-to-list 'load-path "~/.emacs.d/lisp/libs/lsp-ivy")
  (require 'lsp-ivy)
  (setq lsp-ivy-show-symbol-filename nil)
  (define-key lsp-command-map (kbd "s") 'lsp-ivy-workspace-symbol))



;;; init-ivy.el ends here
