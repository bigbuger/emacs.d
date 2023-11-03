;;; global.el --- global config

;;; Commentary:
;;

;;; Code:

;;ivy
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


(require 'pyim-cregexp-utils)
(setq ivy-re-builders-alist
      '((t . pyim-cregexp-ivy)))

(use-package ivy-posframe
  :ensure t
  :init
  (setq ivy-posframe-display-functions-alist
	'((lsp-execute-code-action . ivy-posframe-display-at-point)
          (t                       . ivy-display-function-fallback)))
  :init
  (ivy-posframe-mode 1))

;; end of ivy


;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory '("~/.emacs.d/snippets"))
;;(yas/load-directory yas/root-directory)
(global-set-key (kbd "<M-RET>") 'yas-expand)

(require 'shell)
(dolist (hook (list
               'term-mode-hook
	       'shell-mode-hook
               ))
  (add-hook hook #'(lambda () (yas-minor-mode -1))))

(require 'auto-yasnippet)
(global-set-key (kbd "C-S-w") #'aya-create)
(global-set-key (kbd "C-S-y") #'aya-expand)

(require 'yatemplate)
(setq auto-insert-query nil)
(setq auto-insert-alist nil)
(yatemplate-fill-alist)
(auto-insert-mode t)


;;company
(require 'company)
(global-company-mode 1)
(setq company-minimum-prefix-length 1)
(setq company-require-match 'never)
(setq company-show-quick-access t)
(add-hook 'after-init-hook 'company-quickhelp-mode)

(global-set-key (kbd "<backtab>") 'company-complete)
(global-set-key (kbd "<C-S-tab>") 'company-files)
;; (global-set-key (kbd "<C-M-tab>") 'company-ispell)
(add-to-list 'load-path "~/.emacs.d/lisp/libs/company-english-helper")
(require 'company-english-helper)
(global-set-key (kbd "<C-M-tab>") 'company-english-helper-search)

(setq company-backends
      '(company-semantic
	company-cmake
	company-capf
	company-clang
	company-files
	(company-dabbrev-code company-gtags company-etags company-keywords)))

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  "Add company-yasnippet after BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
(setq company-backends (append company-backends '(company-yasnippet)))


;;projectile
(require 'projectile)
(projectile-mode)
(setq projectile-require-project-root t)
(setq projectile-indexing-method 'hybrid)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(require 'counsel-projectile)
(define-key projectile-command-map (kbd "s") 'counsel-projectile-rg)
(define-key projectile-command-map (kbd "b") 'counsel-projectile-switch-to-buffer)
(define-key projectile-command-map (kbd "f") 'counsel-projectile-find-file)


(setq frame-title-format
      '(""
	"%b"
	(:eval
	 (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s]" project-name))))))


;; ibuffer
(require 'ibuffer-projectile)
(defun ibuffer-projectile-filter ()
  "Set up `ibuffer-projectile'."
  (ibuffer-projectile-set-filter-groups)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook #'ibuffer-projectile-filter)


;; flycheck
;;(eval-after-load 'flymake '(require 'flymake-cursor))
(require 'flycheck)
(global-flycheck-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/libs/flycheck-posframe")
(require 'flycheck-posframe)
(setq flycheck-posframe-warning-prefix "⚠ ")
(setq flycheck-posframe-info-prefix "ℹ ")
(setq flycheck-posframe-error-prefix "⨯ ")

(setq flycheck-posframe-timeout 4)
(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(setq flycheck-indication-mode 'right-fringe)

(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
  (or (alist-get property (alist-get checker flycheck-local-checkers))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(setq flycheck-checker-error-threshold 600)

;; lsp setting
(require 'lsp-mode)
;;(require 'company-lsp)
(require 'lsp-ui)
(require 'dap-mode)

(setq lsp-auto-guess-root t)
(setq lsp-prefer-flymake :none)
(setq lsp-ui-flycheck-enable t)
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

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-signature-function 'lsp-signature-posframe)
(define-key lsp-mode-map (kbd "s-d") 'lsp-signature-activate)
(define-key lsp-mode-map (kbd "s-l l") 'lsp-ui-sideline-mode)

(setq lsp-auto-execute-action nil)
(setq lsp-modeline-code-actions-segments '(count icon))

(setq lsp-completion-provider :none)
(add-hook 'lsp-completion-mode-hook
	  '(lambda ()
	     (setq-local company-backends
			 (cl-adjoin '(company-capf :separate company-yasnippet)
				    company-backends :test #'equal))))

(setq dap-auto-configure-features '(locals controls tooltip))
(define-key lsp-mode-map (kbd "M-?") 'lsp-ui-peek-find-references)
(define-key lsp-mode-map [f5] 'dap-debug)
(define-key lsp-mode-map (kbd "C-<f5>") 'dap-hydra)
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(add-to-list 'load-path "~/.emacs.d/lisp/libs/lsp-ivy")
(require 'lsp-ivy)
(setq lsp-ivy-show-symbol-filename nil)
(define-key lsp-command-map (kbd "s") 'lsp-ivy-workspace-symbol)

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'semgrep-ls))

;; end of lsp setting

(provide 'global)

;;; global.el ends here
