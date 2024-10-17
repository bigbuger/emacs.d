;;; init-treemacs.el --- treemacs 配置
;; treemacs


;;; Commentary:
;; 

(require 'treemacs)
(require 'treemacs-all-the-icons)
;;; Code:

(treemacs-load-theme "all-the-icons")

(setq treemacs-follow-after-init t)
(treemacs-project-follow-mode)
(global-set-key [f8] 'treemacs)

(setq treemacs-filewatch-mode t)
(setq treemacs-file-event-delay 50)
(setq treemacs-no-delete-other-windows nil)

(provide 'init-4-treemacs)

;;; init-4-treemacs.el ends here
