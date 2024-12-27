;;; init-editor-enhance.el --- 编辑增强工具

;;; Commentary:
;; 

;;; Code:


;; 允许对选中区域进行大小写转换
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; crux
(require 'crux)
;; (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line) ;; just use M-m `back-to-indentation'
(global-set-key (kbd "C-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "C-c M-r") #'crux-rename-file-and-buffer)
;; end curx

(use-package vundo
  :ensure t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))


;; smartparens
(require 'smartparens-config)
(add-hook 'prog-mode-hook #'smartparens-mode)
(sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p)) ;; 单词前的字符不要做自动插入引号
(sp-local-pair 'emacs-lisp-mode "'" nil :actions '(wrap))

(define-key smartparens-mode-map (kbd "C-}") 'sp-slurp-hybrid-sexp) ;; 吃掉下一个 sexp
(define-key emacs-lisp-mode-map  (kbd "C-}") 'sp-forward-slurp-sexp) ;; 吃掉一个 sexp
(define-key smartparens-mode-map (kbd "C-{") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c <backspace>") 'sp-raise-sexp) ;; 去掉最高层 sexp
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-clone-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)

;; sexp 新开一行
(defun sp-up-sexp-and-new-line (&optional arg)
  "Jump to end of the sexp the point is in and insert newline.
ARG is pass to `sp-end-of-sexp'"
  (interactive)
  (progn
    (sp-up-sexp)
    (newline-and-indent)))
(global-set-key (kbd "C-<return>") 'sp-up-sexp-and-new-line)

(require 'cl-lib)
(defmacro def-pairs (pairs)
  `(progn
     ,@(cl-loop for (key . val) in pairs
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
;; (global-set-key (kbd "C-c '") 'wrap-with-single-quotes)
(global-set-key (kbd "C-c \"") 'wrap-with-double-quotes)
(global-set-key (kbd "C-c `") 'wrap-with-back-quotes)

;; end smartparens

;; move-text
(require 'move-text)
(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up]   'move-text-up)
(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)
;; end move-text


;; multiple-cursors 多光标编辑
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c M-i") 'mc/insert-numbers)
(define-key mc/keymap (kbd "C-c C-g") 'mc/keyboard-quit)
;; end multiple-cursors


(use-package iedit)


;; expland-regin
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; end expland-regin

;; string-inflection 驼峰/下划线/横线风格自由切换
(require 'string-inflection)
(global-set-key (kbd "C-c u") 'string-inflection-toggle)

(require 'transient)
(transient-define-prefix string-inflection-transient ()
  "String inflection."
  ["string inflection:"
   ("c" "lower camelcase fooVar"  string-inflection-lower-camelcase)
   ("C" "camelcase       FooBar"  string-inflection-camelcase)
   ("u" "underscore      foo_bar" string-inflection-underscore)
   ("U" "upcase          FOO_BAR" string-inflection-upcase)
   ("k" "kebab           foo-bar" string-inflection-kebab-case)
   ])
(global-set-key (kbd "C-c M-u") 'string-inflection-transient)
;; end string-inflection


;; auto-highlight-symbol 高亮展示当前光标下的变量
(require 'auto-highlight-symbol)
(with-eval-after-load 'auto-highlight-symbol
  (setq auto-highlight-symbol-mode-map (make-sparse-keymap)))
(add-hook 'auto-highlight-symbol-mode-hook
	  #'(lambda ()
	      (assq-delete-all 'auto-highlight-symbol-mode minor-mode-map-alist)))

(dolist (hook (list 'emacs-lisp-mode-hook 'scheme-mode-hook 'lisp-mode-hook))
  (add-hook hook #'auto-highlight-symbol-mode))
;; end auto-highlight-symbol-mode


;; 自动保存
(add-to-list 'load-path "~/.emacs.d/lisp/libs/auto-save/")
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-silent t)       ;; 自动保存的时候静悄悄的， 不要打扰我
(setq auto-save-disable-predicates
      '((lambda ()
	  (string-suffix-p ".gpg" (buffer-file-name)))
	(lambda ()
	  (not (string-prefix-p (concat (file-truename "~") "/") (file-truename (buffer-file-name))))) ;; 家目录以外的不要自动保存
	(lambda ()
	  (string-prefix-p (file-truename "~/Library/") (file-truename (buffer-file-name)))) ;; Library 不要自动 保存
	(lambda ()
	  (and (string-prefix-p (concat (file-truename "~") "/.") (file-truename (buffer-file-name)))
	       (not (string-prefix-p (file-truename "~/.emacs.d") (file-truename (buffer-file-name)))))) ;; 其它配置文件不要自动保存
	(lambda ()
	  (not (file-writable-p (buffer-file-name)))) ;; 不可写文件不自动保存
	(lambda ()
	  (tramp-tramp-file-p (buffer-file-name))) ;; tramp 模式不自动保存
	(lambda ()
	  (string-prefix-p "*" (buffer-file-name)))
	(lambda ()
	  (string-prefix-p "#" (buffer-file-name)))))

(require 'dash)
(defvar auto-read-only-patterns `(,(concat (file-truename "~")  "/Library"))
   "File paths matching any pattern in list will be started in `read-only-mode'.")

(defun should-be-read-only-p (file)
  "Return t if FILE should be read-only."
   (-any? 'identity (mapcar (lambda (x) (string-match-p x file)) auto-read-only-patterns)))

(defun auto-read-only-maybe ()
  "Auto make buffer read only."
  (if (should-be-read-only-p (file-truename buffer-file-name)) (read-only-mode t)))

(add-hook 'find-file-hook 'auto-read-only-maybe)

;; end auto-save

;; visual-regexp 正则替换可视化，替换时在原文本中预览替换后的结果
(require 'visual-regexp)
(global-set-key (kbd "C-c r") 'vr/query-replace)

(defun my-vr--minibuffer-setup ()
  "Setup prompt and help when entering minibuffer."
  (when vr--in-minibuffer
    (progn
      (vr--update-minibuffer-prompt)
      (add-hook 'completion-at-point-functions 'elisp-completion-at-point nil t)
      (when vr/auto-show-help (vr--minibuffer-help)))))

(advice-add 'vr--minibuffer-setup :override #'my-vr--minibuffer-setup)

;; end visual-regexp


;; about indent
;; highlight-indent-guides 显示缩进对齐线
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
;; end indent

;; hideshowvis 侧边显示一个折叠的小加号
(add-to-list 'load-path "~/.emacs.d/lisp/libs/hideshowvis/")
(require 'hideshowvis)
(hideshowvis-symbols)
;; (add-hook 'prog-mode-hook 'hideshowvis-enable)
;; end hideshowvis


(provide 'init-0-editor-enhance)

;;; init-0-editor-enhance.el ends here
