;;关闭启动画面
(setq inhibit-startup-message t)
(setq frame-title-format "emacs@%b")


;;关闭蜂鸣
(setq visible-bell t)
;;alway hight light
(global-font-lock-mode 1)

;; undo-tree
(global-undo-tree-mode)

(global-linum-mode 1)
(which-function-mode)
(auto-image-file-mode)

(setq org-src-fontify-natively t)

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

;;显示时间
(display-time)
;=========================================
;(global-set-key (kbd "C-c j") 'goto-line)
;(global-set-key (kbd "M-s") 'query-replace-regexp)

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
;;============================================================
;;窗口之间移到
(global-set-key (kbd "C-c M-n") 'windmove-down)
(global-set-key (kbd "C-c M-p") 'windmove-up)
(global-set-key (kbd "C-c M-f") 'windmove-right)
(global-set-key (kbd "C-c M-b") 'windmove-left)

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


;; (require 'ivy-posframe)
;; (setq ivy-display-function #'ivy-posframe-display)
;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;; (ivy-posframe-enable)

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory "~/.emacs.d/snippets")
;(yas/load-directory yas/root-directory)
(global-set-key (kbd "<M-RET>") 'yas-expand)

(require 'shell)
(dolist (hook (list
               'term-mode-hook
	       'shell-mode-hook
               ))
  (add-hook hook '(lambda () (yas-minor-mode -1))))


;;company
(require 'company)
(global-company-mode 1)
(setq company-minimum-prefix-length 1)
(setq company-require-match nil)
(setq company-show-numbers t)
(add-hook 'after-init-hook 'company-quickhelp-mode)

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
(global-set-key (kbd "C-c p i") 'counsel-projectile-switch-to-buffer)
(global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
(global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)


;; neotree
(require 'neotree)
(require 'all-the-icons)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-vc-integration '(face))
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-autorefresh t)
(setq neo-force-change-root t)
;; (setq projectile-switch-project-action 'neotree-projectile-action)

;;magit
(require 'magit)

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
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;;============================================================
;;flymake基本配置
;;(autoload 'flymake-find-file-hook "flymake" "" t)
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
;;(setq flymake-gui-warnings-enabled nil)	;关闭错误对话框
;;(setq flymake-log-level 0)

;;============================================================

(load-library "realgud")


(require 'multi-term)
(global-set-key (kbd "C-c s") 'multi-term)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(require 'pos-tip)
(require 'osx-dictionary)
(defun osx-dictionary-search-at-point-and-pop ()
  "Search word around and display result with popup."
  (interactive)
  (let* ((word (osx-dictionary--region-or-word))
	 (result (osx-dictionary--search word)))
    (pos-tip-show result)))
(global-set-key (kbd "C-S-d") 'osx-dictionary-search-at-point-and-pop)
