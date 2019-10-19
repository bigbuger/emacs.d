;;; my-rust.el --- rust config


;;; Commentary:
;; 

;;; Code:

(require 'rust-mode)
(require 'racer)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'my-rust)

;;; my-rust.el ends here
