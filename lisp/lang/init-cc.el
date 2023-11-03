;;; init-cc.el --- c mode config

;;; Commentary:
;; 

;;; Code:
(require 'mode-local)
(require 'dap-lldb)
(require 'dap-cpptools)
;;(require 'dap-gdb-lldb)
;;(setq-mode-local c++-mode lsp-prefer-flymake nil lsp-ui-flycheck-enable t)
;;(setq-mode-local c-mode lsp-prefer-flymake nil lsp-ui-flycheck-enable t)

(dolist (hook (list 'c++-mode-hook
		    'c-mode-hook))
  (add-hook hook (lambda ()
		   (progn
		     (c-set-style "k&r")
		     (require 'ccls)
		     (lsp-deferred)))))

(provide 'init-cc)

;;; init-cc.el ends here
