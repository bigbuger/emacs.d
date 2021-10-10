;;; my-agda.el --- agda mode setting

;;; Commentary:
;; 

;;; Code:

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(provide 'my-agda)

;;; my-agda.el ends here
