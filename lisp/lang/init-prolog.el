;;; init-prolog.el --- prolog setting



;;; Commentary:
;; 

;;; Code:

(setq prolog-system 'swi)

(use-package ediprolog
  :after prolog
  :config
  (setq ediprolog-system 'swi)
  :bind (:map prolog-mode-map
	      ("<f5>" . ediprolog-dwim)))

(use-package ob-prolog
  :after org
  :config
  (setq org-babel-prolog-command "swipl")
  (add-to-list 'org-babel-load-languages
	       '(prolog . t)))

(provide 'init-prolog)

;;; init-prolog.el ends here
