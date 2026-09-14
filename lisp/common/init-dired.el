;;; init-dired.el --- dired 文件管理器相关配置

;;; Commentary:
;; 

;;; Code:

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(setq dired-dwim-target 'dired-dwim-target-next-visible) ;; 打开两个窗口时，移动文件默认是移到另一个窗口对应的目录，爽 😊
(setq wdired-allow-to-change-permissions t) ;; 编辑模式允许修改文件权限

;; (setq delete-by-moving-to-trash t) ;; 删除文件时，移到垃圾桶
(setq dired-kill-when-opening-new-dired-buffer t) ;; RET 后仅保留一个 dired buffer

(setq dired-clean-confirm-killing-deleted-buffers nil)

(setq dired-vc-rename-file t) ;; git 支持

(require 'osx-trash) ;; mac 垃圾桶支持
(when (eq system-type 'darwin)
   ;; mac 下时使用 gnu ls
  (setq insert-directory-program "gls"))
(osx-trash-setup)

(setq dired-listing-switches "-a -l -h --time-style=long-iso --group-directories-first") ;; ls 默认参数，size 用xm/xg显示而不是 byte， 长时间格式，将文件夹显示在第一位

;; dired-x
(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  ;; (setq dired-x-hands-off-my-keys nil)
  (define-key dired-mode-map (kbd "M-+") #'dired-create-empty-file)
  )

(setq dired-omit-files "\\`[.][.]?\\'") ;; 隐藏 当前目录和上级目录，就是 . 和 .. 啦
(setq dired-omit-extensions nil) ;; 其它文件默认不隐藏

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-omit-mode 1)
	    (dired-hide-details-mode 1)
	    ))

(use-package dired-du
  :bind (:map dired-mode-map
	      ("K" . dired-du-mode))
  
  :config
  ;; (add-hook 'dired-mode-hook 'dired-du-mode)
  (setq dired-du-size-format t))

;; 文件管理展开子目录
(use-package dired-subtree
  :defer t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
	      ("M-[" . dired-subtree-beginning)
	      ("M-]" . dired-subtree-end)
	      ("M-^" . dired-subtree-up)
	      ("."   . dired-subtree-create-file))
  :config
  (setq dired-subtree-use-backgrounds nil)

  (defun dired-subtree-create-file (file)
     (interactive
      (list (read-file-name "Create file: " (dired-current-directory))))
     (make-empty-file file t)
     (revert-buffer)))

;; dired 菜单
(use-package casual
  :ensure t
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)
	      ("M-o" . #'dired-display-file)
              ("s" . #'casual-dired-sort-by-tmenu)
              ("/" . #'casual-dired-search-replace-tmenu)))


(provide 'init-dired)

;;; init-dired.el ends here
