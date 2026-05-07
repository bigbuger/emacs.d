;;; init-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)
(require 'smartparens)
(require 'org-mouse)
(require 'cl-lib)
(require 'org-id)

(setq org-support-shift-select t
      org-imenu-depth 4
      org-src-fontify-natively t
      org-ellipsis " ⤵ " ;; folding symbol
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-fontify-quote-and-verse-blocks t ;; 引用块也高亮
      org-src-preserve-indentation t
      org-bookmark-names-plist nil ;; 不要把 org 的东西放到书签
      org-id-method 'ts
      )

;; imenu 菜单
(add-hook 'org-mode-hook
	  (lambda ()
	    (imenu-add-menubar-index)))

;; 显示行号
(add-hook 'org-mode-hook #'display-line-numbers-mode)

;; inline显示图片
(setq auto-window-vscroll nil) ;; 关掉，auto-window-vscroll, 不然有图片时按 C-n 要等图片显示完，😩
(setq org-startup-with-inline-images 1)
(setq org-image-actual-width nil) ;; 设置图片自动宽度为 nil 才能用 org_attr 调整

;; 设置图片用系统程序打开
(setq org-file-apps
      (append (mapcar (lambda (ext)
			(cons (concat "\\." ext "\\'")
			      'default))
		      image-file-name-extensions)
	      org-file-apps))

;; 在当前窗口打开 org link
(setf (alist-get 'file org-link-frame-setup) 'find-file)

;; org 内嵌 LaTeX 相关配置
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
      org-startup-with-latex-preview t
      org-preview-latex-image-directory "~/.cache/emacs/data/org/ltximg/"
      org-preview-latex-default-process 'dvisvgm)

;; 添加 latex 头
(with-eval-after-load 'org
  (add-to-list 'org-latex-packages-alist '("" "tikz" t)) ;; 需要 Ghostscript， dvisvgm -V1 检查，mac 要加链接 export LIBGS=/opt/homebrew/lib/libgs.dylib
  (add-to-list 'org-latex-packages-alist '("" "ctex" t)) ;; 支持中文
  (add-to-list 'org-latex-packages-alist '("" "minted")) ;; 支持代码高亮
  (add-to-list 'org-latex-packages-alist '("" "listingsutf8")))


(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)) ;; 预览 tikz

(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(add-to-list 'org-preview-latex-process-alist
	     '(xelatex-ch
	       :programs ("xelatex" "dvisvgm")
	       :description "xdv > svg"
	       :message "You need to install xelatex & dvisvgm"
	       :image-input-type "xdv"
	       :image-output-type "svg"
	       :latex-compiler ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
	       :latex-header "\\RequirePackage{xcolor}\n\\RequirePackage{amsmath}\n\\RequirePackage[fontset=none]{ctex}\n\\setCJKmainfont{PingFang SC}\n"
	       :image-converter ("dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")))
(setq org-preview-latex-default-process 'xelatex-ch)
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

(use-package company-math
  :init
  (add-hook 'org-mode-hook
            (lambda () (setq-local company-backends
				   (cl-adjoin '(company-math-symbols-latex :with company-yasnippet) company-backends :test #'equal))))

  (define-key org-mode-map
	      (kbd "C-\\") 'company-math-symbols-unicode))

;; fragtog 自动光标聚焦到 LaTex 预览时自动转为源码展示
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;; end of LaTeX

;; superstart 美化标题样式
(use-package org-superstar
  :init
  (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '("◉" "○" "◌" "◇" "⬠" "⬡" " "))
  ;; (setq  org-superstar-item-bullet-alist
  ;; 	 '((?* . ?•)
  ;; 	   (?+ . ?⚚)
  ;; 	   (?- . ?➤)))
  (setq org-superstar-cycle-headline-bullets nil)
  :hook
  (org-mode . org-superstar-mode))

;; org-appear 聚焦斜体、删除线等文字格式时自动转为源码展示
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(use-package org-appear
  :init
  (setq org-appear-autolinks 'just-brackets)
  ;; (setq org-appear-autoentities t) ;; 在 latex 中它是整个展开，对于特殊字符不友好
  (setq org-appear-autosubmarkers t)
  (setq org-appear-inside-latex t)
  (setq org-appear-autokeywords t)
  :hook
  (org-mode . org-appear-mode))

;; org-tidy 隐藏 propertize
;; (use-package org-tidy
;;   :ensure t
;;   :config
;;   (setq org-tidy-property-drawer-property-whitelist '("ID" "id"))
;;   (setq org-tidy-property-drawer-property-blacklist '("ROAM_REFS" "roam_refs"))
;;   (setq org-tidy-general-drawer-name-whitelist '("setup" "SETUP"))
;;   (add-hook 'org-mode-hook #'org-tidy-mode))

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
		  '(("#+title:"        . "¶")
		    ("#+TITLE:"        . "¶")
		    ("tags:"           . "☖")
		    ("#+BEGIN_SRC"     . "λ")
                    ("#+END_SRC"       . "▀")
                    ("#+begin_src"     . "λ")
                    ("#+end_src"       . "▀")
		    ("#+BEGIN_QUOTE"   . "✥")
                    ("#+END_QUOTE"     . "▀")
                    ("#+begin_quote"   . "✥")
                    ("#+end_quote"     . "▀")
		    ("#+begin_example" . "✎")
		    ("#+end_example"   . "▀")
		    ("#+BEGIN_EXAMPLE" . "✎")
		    ("#+END_EXAMPLE"   . "▀")
		    ("[ ]"             .  "⬜")
		    ("[X]"             . "✅")
		    ("[-]              . "🟩"")))
	    (prettify-symbols-mode)))

;; header line 展示当前 org 标题
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
  :disabled
  ;; :config
  ;; 美化表格分隔线
  ;; (setq valign-fancy-bar t)
  :hook
  (org-mode . valign-mode))


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

(setq org-babel-python-command "python3")
(setq org-babel-awk-command "gawk")

;; svgbob 一个文本绘图工具 https://github.com/ivanceras/svgbob
(use-package ob-svgbob
  :init
  (add-to-list 'org-src-lang-modes
	       '("svgbob" . artist)))

;; verb 网络请求客户端
(use-package verb
  :demand t
  :init
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(use-package impostman
  :ensure t)

(use-package corg
  :load-path "~/.emacs.d/lisp/libs/corg.el"
  :init
 (add-hook 'org-mode-hook #'corg-setup))

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
	 '((calc . t)
	   (perl . t)
	   (awk . t)
	   (shell . t)
	   (js . t)
	   (passthrough . t)
	   (verb . t)
	   (sql . t)
	   (sqlite . t)
	   (ditaa . t)
	   (svgbob . t))))
;; end of org bale

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

;; 支持 gif 播放
(use-package org-inline-anim
  :hook (org-mode . org-inline-anim-mode))

;; work schedule
(defcustom work-schedule-dir "~/work"
  "工作排期目录."
  :type 'string)

(defcustom work-schedule-jira-link-prefix ""
  "Jira 链接."
  :type 'string)

(defun work-schedule (schedule)
  "工作排期, `SCHEDULE' 工作批次, 列如 2024-Q1B1."
  (interactive
   (let* ((sc (format-time-string "%Y-Q%qB1"))
	  (str (read-string "schedule: " sc 'work-schedule-his)))
     (list str)))
  (find-file (concat work-schedule-dir schedule ".org"))
  (if (= 0 (buffer-size))
      (yas-expand-snippet (yas-lookup-snippet "work schedule"))))

(defalias 'ss 'work-schedule)
;; end work schedule

(use-package orgtbl-aggregate)

(provide 'init-org)

;;; init-org.el ends here
