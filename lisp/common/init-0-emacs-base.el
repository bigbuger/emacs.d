;;; init-emacs-base.el --- emacs 自身基本配置
;;; Commentary:
;; 

;;; Code:

;; 光标样式
(setq-default cursor-type '(bar . 2))

;; 处理 symlinks，不然有一些包在访问 symlinks 时有问题，例如 org mode 的 latex 预览
(setq find-file-visit-truename t)

;; emacs 文件备份系统配置
;; (setq backup-directory-alist '(("." . "~/.cache/emacs/data/backup"))
;;       backup-by-copying t    ; Don't delink hardlinks
;;       version-control t      ; Use version numbers on backups
;;       delete-old-versions t  ; Automatically delete excess backups
;;       )
(setq make-backup-files nil) ;; 基本没用过备份进行恢复，直接禁用了

;; 记录文件上次打开的位置
(require 'saveplace)
(add-hook 'after-init-hook
	  (save-place-mode))

(desktop-save-mode 1)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 150)
(setq recentf-max-saved-items 150)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name "~/.cache/emacs/data/bookmark-default.el"))

;; 硬盘文件更改时，自动同步
(global-auto-revert-mode)

;; 撤销就是撤销，不要 redo
(global-set-key [remap undo] #'undo-only)

(require 'winner)
(winner-mode t)
(global-set-key (kbd "M-s-<left>") #'winner-undo)
(global-set-key (kbd "M-s-<right>") #'winner-redo)

(setq Man-notify-method 'pushy)

;; 关闭启动画面
(setq inhibit-startup-message t)

;; 关闭蜂鸣
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; alway hight light
(global-font-lock-mode 1)

;; 用 y 来回答，不用打全 yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; 不要用 gui 的 弹窗提示
(setq use-dialog-box nil)


;; 保存命令历史
(savehist-mode 1)

;; 搜索显示命中数量
(setq isearch-lazy-count t)


(auto-image-file-mode)

;; 临时 buffer 根据后缀选择 major-mode
(setq-default major-mode
	      (lambda () (if buffer-file-name
			     (fundamental-mode)
			   (let ((buffer-file-name (buffer-name)))
			     (set-auto-mode)))))

;; 显示行号
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode +1)))
(column-number-mode 1)

;; 高亮括号匹配
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; 搜索替换时，不要忽略大小写
(setq case-replace nil)


;; 长行不要切成两个显示
;; (set-default 'truncate-lines t)

;; 选中文字后，粘贴或者输入自动覆盖选中内容，默认是不开，选中后输入还是追加进去，坑 >_<
(delete-selection-mode 1)

;; 允许执行清空 buffer 命令
(put 'erase-buffer 'disabled nil)

;;像素滚动
(pixel-scroll-precision-mode 1)

(setq mouse-wheel-tilt-scroll t)

;; disable bookmark fringe
(setq bookmark-set-fringe-mark nil)

;; whitch function mode
(which-function-mode)
(eval-after-load "which-func"
  '(setq which-func-modes '(emacs-lisp-mode scheme-mode lisp-mode)))

(dolist (hook (list 'emacs-lisp-mode-hook 'scheme-mode-hook 'lisp-mode-hook))
  (add-hook hook
	    (lambda ()
	      (setq header-line-format
		    '((which-func-mode ("" which-func-format " ")))))))

(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-function-mode mode-line-misc-info))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("\\<\\(FIXME\\|fixme\\|TODO\\|todo\\|BUG\\|bug\\)\\>" 1 font-lock-warning-face t)))))

;; enable narrow
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(setq-default display-line-numbers-widen t)

;; make it split horizontal
(setq split-width-threshold 1000)

;; base key ========================================
;;关闭当前缓冲区 Alt+0  ;; C-x 0
(global-set-key (kbd "M-0") 'delete-window)
;;关闭其它缓冲区 Alt+1  ;; C-x 1
(global-set-key (kbd "M-1") 'delete-other-windows)
;;水平分割缓冲区 Alt+2  ;; C-x 2
(global-set-key (kbd "M-2") 'split-window-vertically)
;;垂直分割缓冲区 Alt+3  ;; C-x 3
(global-set-key (kbd "M-3") 'split-window-horizontally)
;; 下一个指令在另外一个窗口执行
(global-set-key (kbd "M-4") 'other-window-prefix)
;; 新开 frame
(global-set-key (kbd "M-5") 'make-frame-command)

;;全屏函数
(defun toggle-fullscreen (&optional f)
  "Fullscreen."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;;强制 align-regexp 使用空格
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;; (setq imenu-max-item-length 80)

;; subword-mode 支持驼峰命名单词移到
;; (add-hook 'prog-mode-hook #'subword-mode)

(provide 'init-0-emacs-base)

;;; init-0-emacs-base.el ends here
