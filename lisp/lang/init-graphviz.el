;;; init-graphviz.el --- graphviz dot setting

;;; Commentary:
;; 

;;; Code:

(use-package graphviz-dot-mode
  :ensure t
  :demand t
  :init
  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages
		 '(dot . t))))

(provide 'init-graphviz)

;;; init-graphviz.el ends here
