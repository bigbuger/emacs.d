;;; init-docker.el --- docker setting


;;; Commentary:
;; 

;;; Code:

(use-package docker
  :ensure t
  :config
  (setq docker-container-shell-file-name "/bin/bash"))

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(provide 'init-docker)

;;; init-docker.el ends here
