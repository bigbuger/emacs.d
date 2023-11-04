;;; init-roam.el --- roam setting
;;; Commentary:
;; 

;;; Code:

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (setq org-directory (file-truename "~/note/roam"))
  (setq org-roam-directory org-directory)
  (setq org-roam-capture-templates '(("d" "default" plain "%?" :target
				      (file+head "${slug}.org" "#+title: ${title}\n")
				      :unnarrowed t)))
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


(provide 'init-roam)

;;; init-roam.el ends here
