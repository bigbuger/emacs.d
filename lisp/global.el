;;关闭启动画面
(setq inhibit-startup-message t)
(setq frame-title-format "%b")

;;关闭蜂鸣
(setq visible-bell t)
;;alway hight light
(global-font-lock-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;;显示时间
(display-time)

;;(global-linum-mode 1)
(global-display-line-numbers-mode 1)
(add-hook 'term-mode-hook
	  (lambda () (display-line-numbers-mode -1)))

(which-function-mode)

(auto-image-file-mode)

(setq org-src-fontify-natively t)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;括号匹配
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ))
;; (require 'smartparens-config)
(show-smartparens-global-mode)
(global-set-key (kbd "C-}") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-{") 'sp-forward-barf-sexp)

(require 'cl)
(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))
(global-set-key (kbd "C-c (") 'wrap-with-parens)
(global-set-key (kbd "C-c [") 'wrap-with-brackets)
(global-set-key (kbd "C-c {") 'wrap-with-braces)
(global-set-key (kbd "C-c '") 'wrap-with-single-quotes)
(global-set-key (kbd "C-c \"") 'wrap-with-double-quotes)
(global-set-key (kbd "C-c `") 'wrap-with-back-quotes)

(require 'move-text)
(move-text-default-bindings)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;========================================
;;关闭当前缓冲区 Alt+4  ;; C-x 0
(global-set-key (kbd "M-4") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)
;;切换到其它缓冲区 Alt+0 ;; C-x o
(global-set-key (kbd "M-0") 'other-window)


(require 'string-inflection)
(global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)
(global-set-key (kbd "C-c u") 'string-inflection-camelcase)

;;ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
(setq ivy-use-selectable-prompt t)
(setq ivy-display-style 'fancy)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f1> k") 'counsel-descbinds)
(global-set-key (kbd "C-c C-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c i") 'counsel-imenu)
(global-set-key (kbd "C-c b") 'counsel-ibuffer)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'ivy-rich)
(ivy-rich-mode 1)

;; (require 'ivy-posframe)
;; (setq ivy-display-function #'ivy-posframe-display)
;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;; (ivy-posframe-enable)

(require 'which-key)
(which-key-mode)

(require 'ace-window)
(global-set-key (kbd "C-c o") 'ace-window)

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory "~/.emacs.d/snippets")
;;(yas/load-directory yas/root-directory)
(global-set-key (kbd "<M-RET>") 'yas-expand)

(require 'shell)
(dolist (hook (list
               'term-mode-hook
	       'shell-mode-hook
               ))
  (add-hook hook '(lambda () (yas-minor-mode -1))))

(require 'auto-yasnippet)
(global-set-key (kbd "C-S-w") #'aya-create)
(global-set-key (kbd "C-S-y") #'aya-expand)


;;company
(require 'company)
(global-company-mode 1)
(setq company-minimum-prefix-length 1)
(setq company-require-match nil)
(setq company-show-numbers t)
(add-hook 'after-init-hook 'company-quickhelp-mode)

(global-set-key (kbd "<backtab>") 'company-complete)

;(add-to-list 'company-backends 'company-yasnippet)
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))



;;projectile
(require 'projectile)
(projectile-mode)
(setq projectile-require-project-root t)
(setq projectile-completion-system 'ivy)


(require 'counsel-projectile)
(global-set-key (kbd "C-c p a") 'counsel-projectile-ag)
(global-set-key (kbd "C-c p r") 'counsel-projectile-rg)
(global-set-key (kbd "C-c p i") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
(global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)


;; neotree
(require 'neotree)
(add-hook 'neotree-mode-hook
	  (lambda () (display-line-numbers-mode -1)))
(require 'all-the-icons)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-vc-integration '(face))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-autorefresh t)
(setq neo-force-change-root t)
;; (setq projectile-switch-project-action 'neotree-projectile-action)
(define-key neotree-mode-map (kbd "+") 'neotree-create-node)
(define-key neotree-mode-map (kbd "<DEL>") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "r") 'neotree-rename-node)

;;magit
(require 'magit)
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)


;; 自动保存
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-slient t)       ;; 自动保存的时候静悄悄的， 不要打扰我

;; visual-regexp
(require 'visual-regexp)
(global-set-key (kbd "C-c r") 'vr/replace)
(global-set-key (kbd "C-c q") 'vr/query-replace)


;;(eval-after-load 'flymake '(require 'flymake-cursor))
(require 'flycheck)
(global-flycheck-mode)
(require 'flycheck-pos-tip)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))


(load-library "realgud")

;; lsp setting
(require 'lsp-mode)
(require 'company-lsp)
(require 'lsp-ui)
(require 'dap-mode)

(setq lsp-prefer-flymake nil)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-sideline-enable nil)

(dap-mode 1)
(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)

(require 'multi-term)
(global-set-key (kbd "C-c s") 'multi-term)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(require 'restclient)
(require 'company-restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; docker
(require 'docker)
(setq docker-container-shell-file-name "/bin/bash")

(require 'pos-tip)
(require 'osx-dictionary)
(defun osx-dictionary-search-at-point-and-pop ()
  "Search word around and display result with popup."
  (interactive)
  (let* ((word (osx-dictionary--region-or-word))
	 (result (osx-dictionary--search word)))
    (pos-tip-show result)))
(global-set-key (kbd "C-S-d") 'osx-dictionary-search-at-point-and-pop)

(require 'centaur-tabs)
(centaur-tabs-mode t)
(setq centaur-tabs-set-icons t)

;;(require 'aggressive-indent)
;;(global-aggressive-indent-mode 1)

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; about dired
(require 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
