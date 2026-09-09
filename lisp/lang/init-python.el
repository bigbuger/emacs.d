;;; init-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:


(require 'dap-python)

(use-package elpy
  :ensure t
  :defer t
  :config
  (delete `elpy-module-highlight-indentation elpy-modules)

  ;; :init
  ;; (advice-add 'python-mode :before 'elpy-enable)

  :config
  (setq elpy-rpc-python-command "python3")

  ;; removes flymake from elpy
  (remove-hook 'elpy-modules 'elpy-module-flymake))


;; (setq python-shell-interpreter "python3"
;;       python-shell-completion-native-disabled-interpreters '("python3")
;;       dap-python-executable "python3")

;; use python-lsp-server
;; pip3 install 'python-lsp-server[all]'
;; (with-eval-after-load "lsp-mode"
;;   (add-to-list 'lsp-disabled-clients 'mspyls)
;;   (add-to-list 'lsp-disabled-clients 'pyright))

(use-package lsp-pyright
  :ensure t
  :init
  (setq lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  (setq lsp-pyright-diagnostic-severity-overrides
	'(("reportAttributeAccessIssue" . "warning")
	  ("reportCallIssue" . "warning")))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
			 (setq-local lsp-enable-imenu nil)
			 (setq-local lsp-inlay-hint-enable t)
                         (lsp))))  ; or lsp-deferred

(setq dap-python-debugger 'debugpy)

;; pip install importmagic
(use-package importmagic
  :ensure t

  :bind
  (:map importmagic-mode-map
	("C-c C-o" . importmagic-fix-symbol-at-point))
  :hook
  (python-mode . importmagic-mode)
  
  :config
  (unbind-key "C-c C-l" importmagic-mode-map))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(python . t)))

(use-package auto-virtualenv
  :config
  (setq auto-virtualenv-verbose t)
  (setq auto-virtualenv-reload-lsp t)
  (auto-virtualenv-setup))

(provide 'init-python)

;;; init-python.el ends here
