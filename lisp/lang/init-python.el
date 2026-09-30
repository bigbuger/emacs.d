;;; init-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))
(add-hook 'python-ts-mode-hook #'superword-mode)
(add-hook 'python-ts-mode-hook
	  #'(lambda ()
              (setq python-indent 4)
              (setq tab-width 4)))

(defun my-python-imenu-format-item-label (type name)
  "Return Imenu label for single node using TYPE and NAME."
  (format "%s %s" name (propertize (format "(:%s)" type) 'face 'font-lock-comment-face)))

(defun my-python-imenu-format-parent-item-label (type name)
  "Return Imenu label for parent node using TYPE and NAME."
  (format "%s " (my-python-imenu-format-item-label type name)))

(setq python-imenu-format-item-label-function #'my-python-imenu-format-item-label)
(setq python-imenu-format-parent-item-label-function #'my-python-imenu-format-parent-item-label)

(with-eval-after-load 'treemacs
  (defun my-py-treemacs--post-process-index(orign index index-mode)
    (if (eq 'python-ts-mode index-mode)
	index
      (funcall orign index index-mode)))
  ;; treemacs--post-process-index always add "Functions" as toplevel tab, so disbale it
  (advice-add 'treemacs--post-process-index :around 'my-py-treemacs--post-process-index))

(add-hook 'python-ts-mode-hook
	  #'(lambda ()
	      (setq-local imenu-space-replacement nil)
	      ;; (setq-local imenu-flatten t)
	      (when (functionp 'consult-dash)
		(setq-local consult-dash-docsets
			    (append '("Python") consult-dash-docsets)))
	      ))

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


(setq python-shell-interpreter "python3"
      python-shell-completion-native-disabled-interpreters '("python3")
      dap-python-executable "python3")

;; chain python-ruff as flycheck checker for python lsp
(require 'lsp-diagnostics)
(flycheck-define-generic-checker 'lsp-python
  "LSP diagnostics checker for Python, cloned from `lsp'."
  :start #'lsp-diagnostics--flycheck-start ;; 复用 lsp-diagnostics 的后端逻辑
  :modes '(python-mode python-ts-mode)
  :predicate (lambda () lsp-diagnostics-mode) ; 与内置 lsp 保持一致的启用条件
  :error-explainer (lambda (e)
                     (lsp-diagnostics-flycheck-error-explainer
                      e (lsp--workspace-server-id (car-safe (lsp-workspaces)))))
  :next-checkers '((t . python-ruff)))

;; only use ruff
(flycheck-remove-next-checker 'python-ruff 'python-mypy)
(add-to-list 'lsp-disabled-clients 'ruff) ;; not as a lsp but flycheck backend, this will speed up some time

(add-to-list 'flycheck-checkers 'lsp-python 'append)

(advice-add 'lsp-diagnostics-flycheck-enable :after
	    (lambda ()
	      (when (derived-mode-p 'python-mode 'python-ts-mode)
		(setq-local flycheck-checker 'lsp-python))))

(add-hook 'python-ts-mode-hook (lambda ()
				 (setq-local lsp-enabled-clients '(ty-ls)
					     lsp-enable-imenu nil
					     lsp-inlay-hint-enable t)
				 (lsp-deferred)))

(setq dap-python-debugger 'debugpy)

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(python . t)))

(with-eval-after-load 'projectile
  (defun my-get-python-run-command (args)
    (let ((default-directory (projectile-project-root)))
      (concat (if (file-exists-p ".venv") "./.venv/bin/python " "python3 ")
	      args)))
  (projectile-update-project-type 'django
				  :compile #'(lambda () (my-get-python-run-command "manage.py collectstatic"))
				  :test #'(lambda () (my-get-python-run-command "manage.py test"))
				  :run #'(lambda () (my-get-python-run-command "manage.py runserver")))

  (add-hook 'python-ts-mode-hook (lambda ()
				   (let ((default-directory (projectile-project-root)))
				     (setq-local flycheck-python-ruff-args
						 `("--config" ,(format "src=[\"%s\", \"%s/src\"]" default-directory default-directory)))
				     (when (file-exists-p ".venv")
				       (setq-local python-shell-virtualenv-root (file-truename ".venv")
						   python-shell-interpreter (concat (file-truename ".venv") "/bin/" "python")
						   dap-python-executable (concat (file-truename ".venv") "/bin/" "python")
						   ))))))

(dap-register-debug-template "Django :: Runserver"
			     (list :type "python"
				   :request "launch"
				   :name "Django :: Runserver"
				   :program "${workspaceFolder}/manage.py"
				   :args "runserver --noreload"
				   :cwd "${workspaceFolder}"))

;; start django vai --noreload: python -m debugpy --listen 0.0.0.0:5678 --wait-for-client manage.py runserver --noreload
(dap-register-debug-template  "Django :: Attach"
			      (list :type "python"
				    :request "attach"
				    :connect (list :host "localhost" :port 5678)
				    :name "Django :: Attach"))

(add-to-list 'load-path "~/.emacs.d/lisp/libs/python-rope.el")
(require 'python-rope)
(with-eval-after-load 'python
  (define-key python-base-mode-map (kbd "C-c <RET>") #'rope-transient))

(provide 'init-python)

;;; init-python.el ends here
