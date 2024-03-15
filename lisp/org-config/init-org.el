;;; init-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)
(require 'smartparens)
(require 'verb)
(require 'org-mouse)
(require 'ob-go)
(require 'cl-lib)


(setq org-support-shift-select t
      org-imenu-depth 4
      org-src-fontify-natively t
      org-ellipsis " ⤵ " ;; folding symbol
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-fontify-quote-and-verse-blocks t ;; 引用块也高亮
      org-src-preserve-indentation t
      org-bookmark-names-plist nil ;; 不要把 org 的东西放到书签
      )

;; inline显示图片
(setq org-startup-with-inline-images 1)
(setq org-image-actual-width nil) ;; 设置图片自动宽度为 nil 才能用 org_attr 调整

;; 设置图片用系统程序打开
(setq org-file-apps
      (append (mapcar (lambda (ext)
			(cons (concat "\\." ext "\\'")
			      'default))
		      image-file-name-extensions)
	      org-file-apps))


;; org 内嵌 LaTeX 相关配置
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
      org-startup-with-latex-preview t
      org-preview-latex-image-directory "~/.cache/emacs/data/org/ltximg/"
      org-preview-latex-default-process 'dvisvgm)

;; 默认添加 tikz 头
(with-eval-after-load 'org
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (add-to-list 'org-latex-packages-alist '("" "ctex" t)) ;; 支持中文
  (add-to-list 'org-latex-packages-alist '("" "minted")) ;; 支持代码高亮
  (add-to-list 'org-latex-packages-alist '("" "listingsutf8")))


(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-listings 'minted)

(add-hook 'org-mode-hook #'smartparens-mode)
(sp-local-pair 'org-mode "\\[" "\\]")

;; ob latex tikz
(setq org-babel-latex-preamble
  (lambda (_)
    "\\documentclass[tikz]{standalone}
"))


;; latex company
(setq org-highlight-latex-and-related '(latex script entities))
(add-hook 'org-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-math-symbols-latex :with company-yasnippet) company-backends :test #'equal))))

(define-key org-mode-map
	    (kbd "C-\\") 'company-math-symbols-unicode)

;; 实时预览 LaTex
(use-package org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/tool/MathJaxNode/node_modules/mathjax-node-cli/bin/tex2svg"))

;; end of LaTeX

;; superstart 美化标题样式
(use-package org-superstar
  :init
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "○" "◇" "⬡" "⯫"))
  (setq org-superstar-cycle-headline-bullets nil)
  :hook
  (org-mode . org-superstar-mode))

;; fragtog 自动光标聚焦到 LaTex 预览时自动转为源码展示
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;; org-appear 聚焦斜体、删除线等文字格式时自动转为源码展示
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(use-package org-appear
  :init
  (setq org-appear-autolinks t)
  ;; (setq org-appear-autoentities t) ;; 在 latex 中它是整个展开，对于特殊字符不友好
  (setq org-appear-autosubmarkers t)
  (setq org-appear-inside-latex t)
  (setq org-appear-autokeywords t)
  (add-hook 'org-mode-hook 'org-appear-mode))

;; org-tidy 隐藏 propertize
(use-package org-tidy
  :ensure t
  :config
  (setq org-tidy-property-drawer-property-whitelist '("ID" "id"))
  (setq org-tidy-property-drawer-property-blacklist '("ROAM_REFS" "roam_refs"))
  (setq org-tidy-general-drawer-name-whitelist '("setup" "SETUP"))
  (add-hook 'org-mode-hook #'org-tidy-mode))

;; 自带折叠, 往 block 加 :hidden
(defun individual-visibility-source-blocks ()
  "Fold some blocks in the current buffer."
  (interactive)
  (org-show-block-all)
  (org-block-map
   (lambda ()
     (let ((case-fold-search t))
       (when (and
              (save-excursion
                (beginning-of-line 1)
                (looking-at org-block-regexp))
              (cl-assoc
               ':hidden
               (cl-third
                (org-babel-get-src-block-info))))
         (org-hide-block-toggle))))))

(add-hook
 'org-mode-hook
 (function individual-visibility-source-blocks))

(setq prettify-symbols-unprettify-at-point t)
;; 自定义 prettify symbol
(add-hook 'org-mode-hook
	  (lambda ()
	    "Beautify Org Symbol"
	    (setq prettify-symbols-alist
		  '(("#+title:" . "¶")
		    ("#+TITLE:" . "¶")
		    ("tags:" . "🏷️")
		    ("#+BEGIN_SRC" . "📝")
                    ("#+END_SRC" . "∎")
                    ("#+begin_src" . "📝")
                    ("#+end_src" . "∎")
		    ("#+BEGIN_QUOTE" . "🔖")
                    ("#+END_QUOTE" . "∎")
                    ("#+begin_quote" . "🔖")
                    ("#+end_src" . "∎")
		    ("[ ]" .  "⬜")
		    ("[X]" . "✅")
		    ("[-] . "🟩"")))
	    (prettify-symbols-mode)))

(use-package org-sticky-header
  :demand t
  :config
  (setq org-sticky-header-full-path 'full)
  (setq org-sticky-header-always-show-header t)
  (setq org-sticky-header-prefix (propertize  "⁖ "
					     'face 'org-level-1))
  :hook
  (org-mode . org-sticky-header-mode))

;; org-colored-text 支持改变字体颜色
(add-to-list 'load-path "~/.emacs.d/lisp/libs/org-colored-text")
(require 'org-colored-text)
(org-add-link-type
 "color"
 (lambda (path)
   (message (concat "color "
                    (progn (add-text-properties
                            0 (length path)
                            (list 'face `((t (:foreground ,path))))
                            path)
			   path))))
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "<span style=\"color:%s;\">%s</span>" path desc))
    ((eq format 'latex)
     (format "{\\color{%s}%s}" path desc)))))


;; valign 表格对齐，支持中英文
(setq org-startup-align-all-tables t)
(use-package valign
  ;; :config
  ;; 美化表格分隔线
  ;; (setq valign-fancy-bar t)
  ;; :hook
  ;;(org-mode . valign-mode)
  )


;; about org balel
(setq org-confirm-babel-evaluate nil)
;; Always redisplay inline images after executing SRC block
(eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))


(defun org-babel-execute:passthrough (body params)
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

(provide 'ob-passthrough)


(setq org-ditaa-jar-path "~/tool/ditaa-0.11.0-standalone.jar")
(setq org-plantuml-exec-mode 'plantuml)

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (calc . t)
   (scheme . t)
   (ruby . t)
   (perl . t)
   (python . t)
   (haskell . t)
   (shell . t)
   (go . t)
   (js . t)
   (passthrough . t)
   (latex . t)
   (dot . t)
   (restclient . t)
   (verb . t)
   (ditaa . t)
   (plantuml . t)
   (sql . t)
   (gnuplot . t)
   (maxima . t)
   (gmpl . t)))
;; end of org bale

;; verb 网络请求客户端
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; gnuplot 用来给表格画图
(use-package gnuplot
  :ensure t)

;; org-download 拖图片自动下载和插入
(use-package org-download
  :ensure t
  :defer t
  :bind (:map org-mode-map
	      ("C-M-y" . org-download-clipboard))
  
  :config
  (setq org-download-annotate-function (lambda (_link) ""))
  (setq-default org-download-image-dir "./image")
  (setq org-download-image-attr-list
        '("#+ATTR_ORG: :width 80% :align center"))
  
  :init
  ;; Add handlers for drag-and-drop when Org is loaded.
  (with-eval-after-load 'org
    (org-download-enable)))

(provide 'init-org)

;;; init-org.el ends here
