;;; init-r.el --- R setting



;;; Commentary:
;; 

;;; Code:

(use-package ess
  :demand t
  :hook
  (ess-r-mode . prettify-symbols-mode))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(R . t)))

(provide 'init-r)

;;; init-r.el ends here
