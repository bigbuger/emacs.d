;;; init-roam.el --- roam setting
;;; Commentary:
;; 

;;; Code:

(use-package org-roam
  :ensure t
  :bind-keymap ("C-c n" . org-roam-command-map)
  :bind-keymap ("s-n" . org-roam-command-map)
  :bind (:map org-roam-command-map
	 ("l" . org-roam-buffer-toggle)
         ("f" . org-roam-node-find)
         ("i" . org-roam-node-insert)
	 ("I" . org-roam-node-insert-immediate)
         ("c" . org-roam-capture)
	 ("e" . org-roam-extract-subtree))
  :init
  (define-prefix-command 'org-roam-command-map)
  
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  (setq org-directory (file-truename "~/note/roam"))
  (setq org-roam-directory org-directory)
  (setq org-roam-capture-templates
	'(("n" "笔记" plain "%?"
	   :if-new (file+head "笔记/${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("f" "闪念随想" plain "%?"
	   :if-new (file+head "闪念随想/${slug}.org" "#+title: ${title}\n#+filetags: :随想:\n")
	   :unnarrowed t)
	  ("r" "摘抄" plain "%?"
	   :if-new (file+head "摘抄/${slug}.org" "#+title: ${title}\n#+filetags: :摘抄:\n")
	   :unnarrowed t)
	  ("m" "备忘" plain "%?"
	   :if-new (file+head "备忘/${slug}.org" "#+title: ${title}\n#+filetags: :备忘:\n")
	   :unnarrowed t)))
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${type:20} ${title:50} "(propertize "${tags:100}" 'face 'org-tag)))
  (setq org-roam-extract-new-file-path "${slug}.org")
  
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (org-roam-db-autosync-mode))

(use-package org-protocol
  :ensure nil)

(use-package org-roam-protocol
  :after org-protocol
  :ensure nil
  :init

  ;; 通过 org-roam-protocl 创建摘录
  ;; 通过 js 保存网页内容:
  ;; javascript:location.href = 'org-protocol://roam-ref?template=r&ref=%27 + encodeURIComponent(location.href) + %27&title=%27 + encodeURIComponent(document.title) + %27&body=%27 + encodeURIComponent(window.getSelection())
  (setq org-roam-capture-ref-templates
        '(("r" "ref"
	   plain "#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"
           :if-new (file+head "摘要/webclip-${slug}.org" "#+title: ${title}\n#+filetags: :摘要:\n")
	   :immediate-finish t
           :unnarrowed t
	   :empty-lines-before 1)))
  (server-start))

(use-package org-roam-ui
  :ensure t
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :bind (:map org-roam-command-map
	      ("u" . org-roam-ui-open))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t
	org-roam-ui-browser-function #'xwidget-webkit-browse-url))


;; xeft 检索笔记
(use-package xeft
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

  :config
  ;; 不想回车创建新文件
  (unbind-key "RET" xeft-mode-map))


(provide 'init-roam)

;;; init-roam.el ends here
