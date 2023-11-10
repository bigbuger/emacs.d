;;; init-ivy.el --- ivy 三件套，搜索神器
;;; Commentary:
;; 

;;; Code:

(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(setq ivy-display-style 'fancy)
(setq counsel-fzf-cmd "fd --type f --hidden --follow --exclude .git --color never '%s'")

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-S-s") 'swiper-isearch)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f1> k") 'counsel-descbinds)
(global-set-key (kbd "C-c M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c e") 'counsel-recentf)
(global-set-key (kbd "C-c b") 'counsel-ibuffer)
(global-set-key (kbd "C-c s") 'counsel-rg)
(global-set-key (kbd "C-c f") 'counsel-fzf)
;;(global-set-key (kbd "C-c j") 'counsel-file-jump)
(global-set-key (kbd "C-c m") 'counsel-bookmark)
(global-set-key (kbd "C-c c") 'counsel-git-checkout)
(global-set-key (kbd "C-x 8") 'counsel-unicode-char)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)


(require 'counsel-fd)
;;(global-set-key (kbd "C-c j") 'counsel-fd-file-jump)
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


;; (require 'pyim-cregexp-utils)
;; (setq ivy-re-builders-alist
;;       '((counsel-find-file . pyim-cregexp-ivy)
;; 	(counsel-recentf . pyim-cregexp-ivy)
;; 	(t . ivy--regex-plus)))

(use-package ivy-posframe
  :ensure t
  :init
  (setq ivy-posframe-display-functions-alist
	'((lsp-execute-code-action . ivy-posframe-display-at-point)
          (t                       . ivy-display-function-fallback)))
  :init
  (ivy-posframe-mode 1))


(provide 'init-ivy)

;;; init-ivy.el ends here
