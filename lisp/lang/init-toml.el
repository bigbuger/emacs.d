;;; init-toml.el --- toml config


;;; Commentary:
;; 

;;; Code:

;; cargo install taplo-cli --features lsp
;; (use-package lsp-mode
;;   :hook
;;   (conf-toml-mode . lsp-deferred))

(use-package topsy
  :hook
  (conf-toml-mode . topsy-mode))

(provide 'init-toml)

;;; init-toml.el ends here
