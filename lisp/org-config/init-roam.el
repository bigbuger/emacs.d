;;; init-roam.el --- roam setting
;;; Commentary:
;; 

;;; Code:

(use-package org-roam
  :ensure t
  :demand t
  :bind-keymap ("C-c n" . org-roam-command-map)
  :config
  (define-key org-roam-command-map "f" '("查询笔记" . org-roam-node-find))
  (define-key org-roam-command-map "i" '("插入笔记链接" . org-roam-node-insert))
  (define-key org-roam-command-map "t" '("插入标签" . org-roam-tag-add))
  (define-key org-roam-command-map "e" '("抽取笔记" . org-roam-extract-subtree))
  (define-key org-roam-command-map "u" '("笔记ui" . org-roam-ui-open))
  (define-key org-roam-command-map "l" '("查看反向链接" . org-roam-buffer-toggle))

  (define-key org-roam-command-map "n" '("生成节点 ID" . org-id-get-create))
  (define-key org-roam-command-map "d" `("删除节点 ID"  . org-id-delete))
  
  (define-key org-roam-command-map "c" '("闪念" . my-roam-capture-flash))
  (define-key org-roam-command-map "r" '("摘要" . my-roam-capture-clip))
  (define-key org-roam-command-map "m" '("备忘" . my-roam-capture-memo))
  
  
  :init
  (define-prefix-command 'org-roam-command-map)
  (add-to-list 'marginalia-command-categories
	       '(org-roam-node-find . node))
  (add-to-list 'completion-category-overrides
	       '(node (styles orderless+initialism+pinyin)))
  
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  (defun org-id-delete ()
    (interactive)
    (org-delete-property "ID"))

  (setq org-directory (file-truename "~/note/roam"))
  (setq org-roam-directory org-directory)
  (setq org-roam-capture-templates
	'(("f" "笔记" plain "%?"
	   :if-new (file+head "笔记/${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("c" "闪念随想" plain "%?"
	   :if-new (file+head "闪念随想/${slug}.org" "#+title: ${title}\n#+filetags: :闪念随想:\n")
	   :unnarrowed t)
	  ("r" "摘抄" plain "%?"
	   :if-new (file+head "摘抄/${slug}.org" "#+title: ${title}\n#+filetags: :摘抄:\n")
	   :unnarrowed t)
	  ("m" "备忘" plain "%?"
	   :if-new (file+head "备忘/${slug}.org" "#+title: ${title}\n#+filetags: :备忘:\n")
	   :unnarrowed t)))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:40} ${file:40} " (propertize "${tags:100}" 'face 'org-tag)))
  (setq org-roam-extract-new-file-path "${slug}.org")
  
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(directory-file-name
         (file-name-directory
          (file-relative-name (org-roam-node-file node) org-roam-directory)))
      (error "")))
  (when (file-exists-p org-roam-directory)
    (org-roam-db-autosync-mode)))

(defun my-roam-filter-by-type (type-name)
  (lambda (node)
    (string-equal type-name (org-roam-node-type node))))


(defun my-roam-capture-by-type (type-name)
  "捕获对应类型的笔记."
  (org-roam-capture-
   :node (org-roam-node-read
          nil
          (my-roam-filter-by-type type-name))
   :templates `(("d" ,type-name plain nil
                 :target (file+head+olp ,(concat type-name "/${slug}.org")
					,(concat "#+title: ${title}\n#+filetags: " ":" type-name ":")
					("")) ;; 通过 olp 加一个空标题让捕获已有文件时，在文件最后，应该有其它办法解决吧😂
		 :empty-lines-before 1
		 :unnarrowed t))))


(defmacro def-my-roam-capture (pairs)
  `(progn
     ,@(cl-loop for (fun-name . type) in pairs
		collect
		`(defun ,(read (concat "my-roam-capture-" (prin1-to-string fun-name)))
		     ()
		   ,(concat "捕获" type)
		   (interactive)
		   (my-roam-capture-by-type ,type)))))

(def-my-roam-capture ((memo . "备忘")
		      (flash . "闪念随想")
		      (clip . "摘抄")))
  


(use-package org-protocol
  :ensure nil)

(use-package org-roam-protocol
  :after org-protocol
  :ensure nil
  :demand t
  :init

  ;; 通过 org-roam-protocl 创建摘录
  ;; 通过 js 保存网页内容:
  ;; javascript:location.href = 'org-protocol://roam-ref?template=r&ref=%27 + encodeURIComponent(location.href) + %27&title=%27 + encodeURIComponent(document.title) + %27&body=%27 + encodeURIComponent(window.getSelection())
  (setq org-roam-capture-ref-templates
        '(("r" "ref"
	   plain "#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
           :if-new (file+head "摘要/${slug}.org" "#+title: ${title}\n#+filetags: :摘要:\n- ref :: %a")
	   :immediate-finish t
           :unnarrowed t
	   :empty-lines-before 1))))

(use-package org-roam-ui
  :ensure t
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  ;; :bind (:map org-roam-command-map
  ;; 	      ("u" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
	org-roam-ui-browser-function #'xwidget-webkit-browse-url))


;; xeft 检索笔记
(use-package xeft
  :demand t
  :bind (:map org-roam-command-map
	      ("s" . xeft))
  :init
  (setq xeft-recursive 'follow-symlinks
	xeft-directory org-roam-directory
	xeft-default-extension "org")

  (defvar-local xeft--displayed-by-xeft-p nil)

  (defun xeft--eager-preview()
    (when-let* ((button (button-at (point)))
		(path (button-get button 'path)))
      ;; Kill previously displayed buffer.
      (when (window-live-p xeft--preview-window)
	(with-selected-window xeft--preview-window
          (when xeft--displayed-by-xeft-p
            (kill-buffer))))
      ;; Show preview of current selection.
      (xeft--preview-file path)))

  (add-hook 'xeft-find-file-hook
            (lambda () (setq xeft--displayed-by-xeft-p t)))

  (advice-add 'xeft-next :after #'xeft--eager-preview)
  (advice-add 'xeft-previous :after #'xeft--eager-preview)

  (defvar xeft-prompt "🔎: ")
  
  (defun xeft-add-prompt ()
    (progn
      (goto-char (point-min))
      (insert (propertize xeft-prompt
			  'face 'font-lock-keyword-face
			  'intangible t
			  'read-only t
			  'composition t
			  'front-sticky t
			  'rear-nonsticky t))))
  
  (add-hook 'xeft-mode-hook 'xeft-add-prompt)
  (add-hook 'xeft-mode-hook
	    #'(lambda ()
		(setq header-line-format
		      (concat "Search node. Can use " (propertize "`AND' `OR' `XOR' `NOT'" 'face 'font-lock-keyword-face) "."))))
  
  :config
  ;; 不想回车创建新文件
  (unbind-key "RET" xeft-mode-map)

  (setq xeft-ignore-extension image-file-name-extensions)
  
  ;; 配合 xeft-add-prompt，覆盖原来读取查询语句的方法，跳过 xeft-prompt, 一定要放在 :config 才能覆盖, use-package 懒加载
  (defun xeft--get-search-phrase ()
    "Return the search phrase. Assumes current buffer is a xeft buffer."
    (save-excursion
      (goto-char (point-min))
      (forward-char (length xeft-prompt))
      (string-trim
       (buffer-substring-no-properties (point) (line-end-position))))))

(use-package embark-org-roam
  :load-path "~/.emacs.d/lisp/libs/embark-org-roam"
  :after (org-roam embark)
  :config
  (setq embark-org-roam-readonly t))


(provide 'init-roam)

;;; init-roam.el ends here
