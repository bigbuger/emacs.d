;;; init-r.el --- R setting



;;; Commentary:
;; 

;;; Code:

(use-package ess
  :demand t
  :hook
  (ess-r-mode . prettify-symbols-mode))

(provide 'init-r)

;;; init-r.el ends here
