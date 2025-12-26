;;; init-editor-enhance.el --- 编辑增强工具

;;; Commentary:
;; 

;;; Code:


;; 允许对选中区域进行大小写转换
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq duplicate-line-final-position -1)
(setq duplicate-region-final-position -1)
(global-set-key (kbd "C-d") #'duplicate-dwim)

;; crux
(require 'crux)
;; (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line) ;; just use M-m `back-to-indentation'
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "C-c M-r") #'crux-rename-file-and-buffer)
;; end curx

(defun insert-current-file-name-at-point (&optional full-path)
  "Insert the current filename at point.
With prefix argument, use full path."
  (interactive "P")
  (let* ((buffer
	  (if (minibufferp)
	      (window-buffer
	       (minibuffer-selected-window))
	    (current-buffer)))
	 (filename (buffer-file-name buffer)))
    (if filename
	(insert (if full-path filename (file-name-nondirectory filename)))
      (error (format "Buffer %s is not visiting a file" (buffer-name buffer))))))

(global-set-key (kbd "M-o") #'insert-current-file-name-at-point)

(use-package vundo
  :ensure t
  :bind
  (("C-x u" . vundo))
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
(define-key emacs-lisp-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key emacs-lisp-mode-map (kbd "C-M-e") 'sp-end-of-sexp)
(define-key lisp-interaction-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key lisp-interaction-mode-map (kbd "C-M-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-clone-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)

(setopt delete-pair-blink-delay 0.1)
(global-set-key (kbd "C-M-z") 'delete-pair)
(define-key smartparens-mode-map (kbd "C-M-z") 'sp-unwrap-sexp)

;; sexp 新开一行
(defun sp-up-sexp-and-new-line (&optional arg)
  "Jump to end of the sexp the point is in and insert newline.
ARG is pass to `sp-end-of-sexp'"
  (interactive)
  (progn
    (sp-up-sexp)
    (newline-and-indent)))
(define-key emacs-lisp-mode-map (kbd "M-<return>") 'sp-up-sexp-and-new-line)

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


(use-package drag-stuff
  :demand t
  :bind
  (("s-<up>" . drag-stuff-up)
   ("s-<down>" . drag-stuff-down)
   ("s-<right>" . drag-stuff-right)
   ("s-<left>" . drag-stuff-left))
  :init
  (drag-stuff-global-mode 1)

  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
	(indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  
  (advice-add 'drag-stuff-up :after 'indent-region-advice)
  (advice-add 'drag-stuff-down :after 'indent-region-advice))

(setq cua-enable-cua-keys nil)
;; (setq cua-highlight-region-shift-only t) ;; no transient mark mode
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(cua-mode t)
(unbind-key "C-<return>" 'cua-global-keymap)
(define-key cua-global-keymap (kbd "C-S-<return>") #'cua-set-rectangle-mark)

;; kmacro-x 利用键盘宏支持选择多个匹配地方进行编辑
(use-package kmacro-x
  :ensure t
  :init
  (require 'kmacro-x-mc)

  (global-set-key (kbd "C-<")  #'kmacro-x-mc-mark-previous)
  (global-set-key (kbd "C->") #'kmacro-x-mc-mark-next)
  (global-set-key (kbd "M-<mouse-1>") #'kmacro-x-mc-mark-at-click)
  
  (setq kmacro-x-mc-live-preview t)
  (define-key kmacro-x-mc-mode-map (kbd "C-<return>") #'kmacro-x-mc-apply)
  (unbind-key "RET" kmacro-x-mc-mode-map)
  )
 
;; dmacro 动态生成键盘宏
;; 重复一套操作两次后直接用 `C-S-e' 后就直接调用
;; 例如输入 hello he 之后按 `C-S-e' 会变成 hello hello
(use-package dmacro
  :ensure t
  :init
  (global-dmacro-mode t))

;; expland-regin
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; end expland-regin

;; string-inflection 驼峰/下划线/横线风格自由切换
(require 'string-inflection)
(global-set-key (kbd "M-c") 'string-inflection-lower-camelcase) ;; orig capitalize-word
(global-set-key (kbd "M-C") 'string-inflection-camelcase) ;; orig capitalize-word
(global-set-key (kbd "M-u") 'string-inflection-underscore) ;; orig upcase-word
(global-set-key (kbd "M-U") 'string-inflection-upcase) ;; orig upcase-word

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
(global-set-key (kbd "C-c u") 'string-inflection-transient)
;; end string-inflection

(use-package symbol-overlay
  :hook
  (emacs-lisp-mode . symbol-overlay-mode)
  (lisp-mode . symbol-overlay-mode)
  (scheme-mode . symbol-overlay-mode))


;; 自动保存
(add-to-list 'load-path "~/.emacs.d/lisp/libs/auto-save/")
(require 'auto-save)            ;; 加载自动保存模块
(auto-save-enable)              ;; 开启自动保存功能
(setq auto-save-silent t)       ;; 自动保存的时候静悄悄的， 不要打扰我
(setq auto-save-disable-predicates
      '((lambda ()
	  (and
	   (string-suffix-p ".gpg" (buffer-file-name))
	   (not epa-file-encrypt-to))) ;; 不知道密钥时，总是问加密密钥，老烦了
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

;; visual-replace 替换可视化，替换时在原文本中预览替换后的结果
(use-package visual-replace
  :defer t
  :bind (("C-c r" . visual-replace-regexp)
	 ("C-c R" . visual-replace-thing-at-point)
         :map isearch-mode-map
         ("C-c r" . visual-replace-from-isearch)
	 :map visual-replace-mode-map
	 ("C-n" . visual-replace-next-match)
	 ("C-p" . visual-replace-prev-match)
	 ("M-<RET>" . visual-replace-apply-one))
  ;; :hook ((visual-replace-minibuffer-mode . visual-replace-toggle-query)) ;; FIXME using visual-replace-apply-one also query, I don't know why
  :config
  (setq visual-replace-default-to-full-scope t)
  (define-key visual-replace-mode-map (kbd "C-o")
	      visual-replace-secondary-mode-map)
  (define-key visual-replace-transient-map (kbd "C-n") #'visual-replace-next-match)
  (define-key visual-replace-transient-map (kbd "C-p") #'visual-replace-prev-match)
  
  (setq visual-replace-display-total t)
  (setq visual-replace-min-length 1))

(defun query-replace-read-to-with-completion (orign &rest args)
  (minibuffer-with-setup-hook
      (:append
       (lambda ()
	 (add-hook 'completion-at-point-functions
		   #'elisp-completion-at-point nil t)))
    (apply orign args)))
(advice-add 'query-replace-read-to :around #'query-replace-read-to-with-completion)
;; end visual-replace

;; isearch-mb 有更好的搜索体验, 可以直接按箭头移到，也不用搞 isearch 复杂的编辑按钮
(use-package isearch-mb
  ;; :bind (("C-s" . isearch-forward-regexp) ;; just "C-M-s"
  ;; 	 ("C-r" . isearch-backward-regexp)) ;; just "C-M-r"

  :config
  (add-to-list 'isearch-mb--with-buffer #'isearch-yank-word)
  (add-to-list 'isearch-mb--after-exit #'isearch-occur)
  (add-to-list 'isearch-mb--after-exit #'visual-replace-from-isearch)
  (define-key isearch-mb-minibuffer-map (kbd "C-w") #'isearch-yank-word)
  (define-key isearch-mb-minibuffer-map (kbd "C-c r") #'visual-replace-from-isearch)

  :init
  (isearch-mb-mode))


;; about indent
;; highlight-indent-guides 显示缩进对齐线
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
;; end indent

;; hideshow
(use-package hideshow
  :bind
  (:map hs-minor-mode
	("S-<tab>" . hs-toggle-hiding)))
;; end hideshow


;; This package provides helper functions to delete, rename, or copy buffer files:
;; 
;; bufferfile-rename: Renames the file visited by the current buffer and updates the buffer name for all associated buffers, including clones/indirect buffers. It also ensures that buffer-local features referencing the file, such as Eglot or dired buffers, are correctly updated to reflect the new file name.
;; bufferfile-delete: Delete the file associated with a buffer and kill all buffers visiting the file, including clones/indirect buffers.
;; bufferfile-copy: Copies the file visited by the current buffer to a new file.
;; The functions above also ensures that any modified buffers are saved prior to executing operations like renaming, deleting, or copying.
(use-package bufferfile
  :ensure t
  :custom
  ;; If non-nil, display messages during file renaming operations
  (bufferfile-verbose nil)

  ;; If non-nil, enable using version control (VC) when available
  (bufferfile-use-vc nil)

  ;; Specifies the action taken after deleting a file and killing its buffer.
  (bufferfile-delete-switch-to 'parent-directory)
  :bind (("C-c M-d" . bufferfile-delete)
	 ("C-c M-c" . bufferfile-copy)))

;; grep 分组
(setq grep-use-headings t)

(use-package wgrep
  :bind (:map grep-mode-map
	      ("e" . wgrep-change-to-wgrep-mode)))

(require 'rg)
(rg-define-search my-rg-project
  :query ask
  :format regexp
  :files "everything"
  :confirm prefix
  :dir project)
(global-set-key (kbd "M-s r") #'rg-menu)

(use-package avy
  :bind
  (("C-c j"	.	avy-goto-word-1)
   ("M-g w"	.	avy-goto-word-or-subword-1)
   ("M-g M-w"	.	avy-goto-word-or-subword-1)
   ("M-g l"	.	avy-goto-line)
   ("M-g M-l"	.	avy-goto-line)))

(provide 'init-0-editor-enhance)

;;; init-0-editor-enhance.el ends here
