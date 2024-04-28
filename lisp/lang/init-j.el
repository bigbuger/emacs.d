;;; init-j.el --- J language setting

;; Add this to your emacs config

;;; Commentary:
;; 

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/libs/j-mode")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
(setq j-console-cmd "jconsole")

(provide 'init-j)

;;; init-j.el ends here
