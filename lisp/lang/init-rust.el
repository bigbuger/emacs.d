;;; init-rust.el --- rust config


;;; Commentary:
;; 

;;; Code:

(require 'rust-mode)
(require 'dap-gdb-lldb)
(require 'rustic)
;;(require 'racer)

;;(add-hook 'rust-mode-hook #'racer-mode)
;;(add-hook 'racer-mode-hook #'eldoc-mode)

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(setq lsp-rust-server 'rust-analyzer)
(add-hook 'rust-mode-hook
	  (lambda ()
	    (lsp)))

(dap-register-debug-template
 "Rust::LLDB Run Configuration"
 (list :type "lldb"
       :request "launch"
       :name "LLDB::Run"
       :gdbpath "rust-lldb"
       :target nil
       :cwd nil))



(provide 'init-rust)


;;; init-rust.el ends here
