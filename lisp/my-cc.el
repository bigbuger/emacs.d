;;; my-cc.el --- c mode config

;;; Commentary:
;; 

;;; Code:

(setq-mode-local c++-mode lsp-prefer-flymake nil lsp-ui-flycheck-enable t)
(setq-mode-local c-mode lsp-prefer-flymake nil lsp-ui-flycheck-enable t)
(add-hook 'c++-mode-hook 'lsp-mode)
(add-hook 'c-mode-hook 'lsp-mode)


(provide 'my-cc)

;;; my-cc.el ends here
