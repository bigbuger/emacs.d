;;; init-toml.el --- toml config


;;; Commentary:
;; 

;;; Code:

(use-package lsp-mode
  :hook
  (conf-toml-mode . lsp-deferred))

(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(provide 'init-toml)

;;; init-toml.el ends here
