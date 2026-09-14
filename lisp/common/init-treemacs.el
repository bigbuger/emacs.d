;;; init-treemacs.el --- treemacs 配置
;; treemacs


;;; Commentary:
;; 

(require 'treemacs)
;;; Code:

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

;; (setq treemacs-follow-after-init t)
(treemacs-project-follow-mode)
(global-set-key [f8] 'treemacs)

(setq treemacs-filewatch-mode t)
(setq treemacs-file-event-delay 50)
(setq treemacs-no-delete-other-windows nil)
(setq treemacs-width-is-initially-locked nil)

(provide 'init-treemacs)

;;; init-treemacs.el ends here
