;;; my-org.el --- org mode config

;;; Commentary:
;; 

;;; Code:

(require 'org)

(setq org-src-fontify-natively t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
	(scheme . t)
        (ruby . t)
        (python . t)
        (shell . t)
        (latex . t)))

(provide 'my-org)

;;; my-org.el ends here
