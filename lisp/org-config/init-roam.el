;;; init-roam.el --- roam setting
;;; Commentary:
;; 

;;; Code:

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n c" . org-roam-capture)
	 ("C-c n t" . org-roam-tag-add))
  :init
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
  
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
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


(provide 'init-roam)

;;; init-roam.el ends here
