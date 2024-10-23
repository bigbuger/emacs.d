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
		   (c-set-style "k&r"))))

(use-package ccls
  :init
  (dolist (hook (list 'c++-mode-hook
		      'c-mode-hook))
    (add-hook hook (lambda ()
		     (progn
		       (setq-local lsp-enable-indentation nil)
		       (require 'ccls)
		       (lsp-deferred))))))

(use-package cmake-mode)

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
 	       '(C . t)))

(provide 'init-cc)

;;; init-cc.el ends here
