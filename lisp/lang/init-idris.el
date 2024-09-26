;;; init-idris.el --- idris editor



;;; Commentary:
;; 

;;; Code:

(use-package idris-mode
  :ensure t

  :custom
  (idris-interpreter-path "idris2")

  :config
  (require 'flycheck-idris) ;; Syntax checker
  (add-hook 'idris-mode-hook #'flycheck-mode))

(provide 'init-idris)

;;; init-idris.el ends here
