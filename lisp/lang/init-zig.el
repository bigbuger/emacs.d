;;; init-zig.el --- zig


;;; Commentary:
;; 

;;; Code:

(use-package zig-mode
  :init
  (autoload 'zig-mode "zig-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-mode))

  :hook
  (zig-mode . lsp))

(provide 'init-zig)

;;; init-zig.el ends here
