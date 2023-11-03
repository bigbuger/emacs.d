;;; init-dired.el --- dired 文件管理器相关配置

;;; Commentary:
;; 

;;; Code:

;; (require 'all-the-icons-dired)
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(setq dired-dwim-target t) ;; 打开两个窗口时，移动文件默认是移到另一个窗口对应的目录，爽 😊
(setq wdired-allow-to-change-permissions t) ;; 编辑模式允许修改文件权限

(setq delete-by-moving-to-trash t) ;; 删除文件时，移到垃圾桶
 
(require 'osx-trash) ;; mac 垃圾桶支持
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls") ;; mac 下时使用 gnu ls
  (osx-trash-setup))

(setq dired-listing-switches "-l -h --time-style=long-iso --group-directories-first") ;; ls 默认参数，size 用xm/xg显示而不是 byte， 长时间格式，将文件夹显示在第一位

;; dired-x
(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Set dired-x global variables here.  For example:
  ;; (setq dired-guess-shell-gnutar "gtar")
  ;; (setq dired-x-hands-off-my-keys nil)
  )

(setq dired-omit-files "\\`[.][.]?\\'") ;; 隐藏 当前目录和上级目录，就是 . 和 .. 啦
(setq dired-omit-extensions nil) ;; 其它文件默认不隐藏

(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    (dired-omit-mode 1) 
	    ))

;; dirvish 很好用的文件管理实现 😊
(require 'dirvish)
(dirvish-override-dired-mode)
(setq dirvish-attributes '(vc-state all-the-icons subtree-state file-size))
(setq dirvish-mode-line-format nil)

(define-key dirvish-mode-map
	    (kbd "TAB") 'dirvish-subtree-toggle) ;; 展开下级目录
(define-key dirvish-mode-map
	    (kbd "C-c C-t") 'dirvish-layout-toggle) ;; 打开预览面板
(define-key dirvish-mode-map
	    (kbd ".") 'dired-create-empty-file) ;; .快速新加文件
(define-key dirvish-mode-map
	    (kbd "/") 'dirvish-fd) ;; / 用 fd 查询文件
(define-key dirvish-mode-map
	    (kbd "M-l") 'dirvish-ls-switches-menu)


(add-hook 'dirvish-find-entry-hook
	  (lambda (&rest _) (setq-local truncate-lines t)));; 不要自动折行 

(setq dirvish-reuse-session t)


(provide 'init-dired)

;;; init-dired.el ends here
