;;; my-liquid.el --- setting for editing liquid


;;; Commentary:
;; 

;;; Code:
(require 'lsp-mode)

(define-derived-mode liquid-mode web-mode "Liquid"
  "Use web mode to highlight liquid files.")
(provide 'liquid-mode)
(add-to-list 'auto-mode-alist '("\\.liquid\\'" . liquid-mode))

;; Shopify template lsp with theme-check
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(liquid-mode . "\\.liquid\\'"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "theme-check-language-server")
                     :activation-fn (lsp-activate-on "\\.liquid\\'")
                     :server-id 'theme-check)))

(add-hook 'liquid-mode-hook #'lsp-deferred)

(provide 'my-liquid)

;;; my-liquid.el ends here
