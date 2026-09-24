;;; init-lua.el --- lua
;;; Commentary:
;; 

;;; Code:

(defun lsp-clients-emmylua-ls-test ()
  "Test the Emmy Lua binaries and files."
  (executable-find "emmylua_ls"))

(defun lsp-emmylua-ls--download-server (_client callback error-callback _update?)
  "Install/update emmylua_ls language server using `cargo install'."
  (lsp-async-start-process
   callback
   error-callback
   "cargo" "install" "emmylua_ls"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection "emmylua_ls"
                                        #'lsp-clients-emmylua-ls-test)
  :activation-fn (lsp-activate-on "lua")
  :priority 0
  :server-id 'emmylua-ls
  :download-server-fn #'lsp-emmylua-ls--download-server))

(with-eval-after-load 'lua-mode
  (add-hook 'lua-mode-hook #'lsp))

(provide 'init-lua)

;;; init-lua.el ends here
