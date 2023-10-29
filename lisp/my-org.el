;;; my-org.el --- org mode config

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
      org-src-preserve-indentation t)

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


;; loclization time andcalendar
(setq system-time-locale "zh_CN")
(setq calendar-week-start-day 1)
(use-package cal-china-x)

;; agenda
(require 'org-agenda)
(global-set-key (kbd "C-c a") #'org-agenda)

;; 加载日程文件
(defun org-set-agenda-files-recursively (dir)
  "Set agenda files from root DIR."
  (setq org-agenda-files
    (directory-files-recursively dir "\.org$")))

(defcustom org-agenda-root-dir "~/Documents/工作"
  "The root dir of `org-agenda'."
  :type 'string)

(org-set-agenda-files-recursively org-agenda-root-dir)
(defun org-load-agenda-files ()
  (interactive)
  (org-set-agenda-files-recursively org-agenda-root-dir))

;;日程显示日期为中文

(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (aref cal-china-x-days
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date)))
    (format "%04d-%02d-%02d 周%s" year month day dayname)))


;; end agenda


;; org 内嵌 LaTeX 相关配置
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5)
      org-preview-latex-image-directory "~/.emacs.d/.org/ltximg/"
      org-preview-latex-default-process 'dvisvgm)

(add-hook 'org-mode-hook #'smartparens-mode)
(sp-local-pair 'org-mode "\\[" "\\]")

;; latex company
(setq org-highlight-latex-and-related '(latex script entities))
(add-hook 'org-mode-hook
          (lambda () (setq-local company-backends
				 (cl-adjoin '(company-math-symbols-latex :with company-yasnippet) company-backends :test #'equal))))
;; end of LaTeX

;; superstart 美化标题样式
(use-package org-superstar
  :init
  (setq org-superstar-remove-leading-stars t)
  :hook
  (org-mode . org-superstar-mode))

;; fragtog 自动光标聚焦到 LaTex 预览时自动转为源码展示
(use-package org-fragtog
  :hook
  (org-mode . org-fragtog-mode))

;; org-appear 聚焦斜体、删除线等文字格式时自动转为源码展示
(setq org-hide-emphasis-markers t)
(use-package org-appear
  :init
  (add-hook 'org-mode-hook 'org-appear-mode)
  (setq org-appear-autolinks t)
  (setq org-appear-autokeywords t))

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
		    ("[ ]" .  "⬜")
		    ("[X]" . "✅")
		    ("[-] . "🟩"")))
	    (prettify-symbols-mode)))

;; org-colored-text 支持改变字体颜色
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
   (sql . t)))
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

;; about org-roam
(use-package org-roam
  :ensure t
  :init
  (progn
    (setq org-directory (file-truename "~/note/roam"))
    (setq org-roam-directory org-directory)
    (setq org-id-locations-file (concat org-directory "/.org-id-locations"))
    (setq org-roam-capture-templates '(("d" "default" plain "%?" :target
					(file+head "${slug}.org" "#+title: ${title}\n#+filetags: \n")
					:unnarrowed t))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:50} "(propertize "${tags:100}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(use-package org-roam-ui
  :ensure t
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :bind (("C-c n u" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
	org-roam-ui-browser-function #'xwidget-webkit-browse-url))

;; end of org-roam

(use-package org-ql)


(provide 'my-org)

;;; my-org.el ends here
