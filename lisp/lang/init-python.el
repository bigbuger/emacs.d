;;; init-python.el --- pytohon config
;;;

;;; Commentary:
;; 

;;; Code:

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

(defun my-python-ts-get-method-name (node)
  "Return name of `class method'."
  (let*  ((decorated-node (treesit-parent-until node
						(lambda (p)
						  (string-equal "decorated_definition"
								(treesit-node-type p)))))
	  (decorator-text (cl-find-if
			   (lambda (text) (member text '("classmethod" "staticmethod")))
			   (mapcar (lambda (d)
				     (treesit-node-text (cl-second (treesit-node-children d)) t))
				   (treesit-filter-child decorated-node
							 (lambda (d)
							   (string-equal "decorator" (treesit-node-type d)))))))
	  (method (concat (treesit-node-text
			   (treesit-node-child-by-field-name node "name"))
			  (when decorator-text
			    (concat " "
				    (propertize decorator-text 'face 'font-lock-comment-face)))))
	  (class-node (treesit-parent-until node
					    (lambda (p)
					      (string-equal "class_definition"
							    (treesit-node-type p)))))
	  (class (when class-node (treesit-node-text (treesit-node-child-by-field-name class-node "name")))))
    (if class
	(concat class "." method)
      method)))

(add-hook 'python-ts-mode-hook
	  #'(lambda ()
	      (setq-local imenu-create-index-function #'treesit-simple-imenu)
	      (setq-local treesit-simple-imenu-settings
			  `(("Class" "\\`class_definition\\'" nil nil)
			    ("Function" "\\`function_definition\\'" nil my-python-ts-get-method-name)))))

(with-eval-after-load 'consult-imenu
  (add-to-list 'consult-imenu-config
	       '(python-ts-mode
		 :toplevel "Function"
		 :types ((?f "Function" font-lock-function-name-face)
			 (?c "Class" font-lock-type-face)))))

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
;; pip install 'python-lsp-server[all]'
;; pip install pylsp-rope # for code action
;; pip install pylsp-workspace-symbols # for call_hierarchy
(setq use-pylsp nil) ;; yep, too slow
(use-package lsp-mode
  :if use-pylsp
  :init
  (add-to-list 'lsp-disabled-clients 'mspyls)
  (add-to-list 'lsp-disabled-clients 'pyright)
  (add-to-list 'lsp-disabled-clients 'ruff)

  (setopt lsp-pylsp-plugins-mypy-enabled t
	  lsp-pylsp-plugins-mypy-dmypy t
	  lsp-pylsp-plugins-mypy-live-mode nil)

  (lsp-register-custom-settings
   '(("pylsp.plugins.jedi_workspace_symbols.enable" t t)
     ("pylsp.plugins.call_hierarchy.enable" t t)
     ("pylsp.plugins.rope_autoimport.enabled" nil t) ;; maybe slow
     ("pylsp.plugins.rope_autoimport.completions.enabled" nil t)))

  (add-hook 'python-mode-ts-hook
	    #'(lambda ()
		(setq-local lsp-enable-imenu nil)
		(setq-local lsp-inlay-hint-enable t)
		(lsp))))

(use-package lsp-mode
  :ensure t
  :config
  (lsp-register-custom-settings
   '(;; ===== 彻底禁用 Jedi =====
     ("pylsp.plugins.jedi_completion.enabled" false t)
     ("pylsp.plugins.jedi_definition.enabled" false t)
     ("pylsp.plugins.jedi_hover.enabled" false t)
     ("pylsp.plugins.jedi_references.enabled" false t)
     ("pylsp.plugins.jedi_signature_help.enabled" false t)
     ("pylsp.plugins.jedi_symbols.enabled" false t)
     ("pylsp.plugins.jedi.environment" nil t)
     ("pylsp.plugins.jedi.extra_paths" [] t)

     ;; ===== 禁用 Rope 的非重构功能 =====
     ("pylsp.plugins.rope_completion.enabled" false t)
     ("pylsp.plugins.rope_rename.enabled" false t)
     ("pylsp.plugins.rope_autoimport.completions.enabled" false t)

     ;; ===== 只启用 Rope 的 Code Action =====
     ("pylsp.plugins.rope_refactor.enabled" true t)
     ("pylsp.plugins.rope_autoimport.code_actions.enabled" true t)

     ;; ===== 禁用所有诊断/格式化 =====
     ("pylsp.plugins.flake8.enabled" false t)
     ("pylsp.plugins.mccabe.enabled" false t)
     ("pylsp.plugins.pycodestyle.enabled" false t)
     ("pylsp.plugins.pydocstyle.enabled" false t)
     ("pylsp.plugins.pyflakes.enabled" false t)
     ("pylsp.plugins.pylint.enabled" false t)
     ("pylsp.plugins.yapf.enabled" false t)
     ("pylsp.plugins.autopep8.enabled" false t)
     ("pylsp.plugins.black.enabled" false t)
     ("pylsp.plugins.isort.enabled" false t)))
  :hook (python-ts-mode . lsp))


(use-package lsp-pyright
  :unless use-pylsp
  :ensure t
  :init
  (setq lsp-pyright-langserver-command "basedpyright") ;; or pyright
  (setq lsp-pyright-type-checking-mode "basic")
  (setq lsp-pyright-diagnostic-severity-overrides
	'(("reportMissingTypeStubs"		.	"hint")
	  ("reportMissingParameterType"		.	"hint")
	  ("reportArgumentType"			.	"warning")
	  ("reportAssignmentType"		.	"warning")
	  ("reportAttributeAccessIssue"		.	"warning")
	  ("reportCallIssue"			.	"warning")
	  ("reportFunctionMemberAccess"		.	"warning")
	  ("reportGeneralTypeIssues"		.	"warning")
	  ("reportIncompatibleMethodOverride"	.	"warning")
	  ("reportInvalidTypeForm"	        .	"warning")
	  ("reportOptionalMemberAccess"	        .	"warning")
	  ("reportRedeclaration"		.	"warning")
	  ("reportReturnType"			.	"warning")))
  :hook (python-ts-mode . (lambda ()
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
  (python-ts-mode . importmagic-mode)
  
  :config
  (unbind-key "C-c C-l" importmagic-mode-map))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
	       '(python . t)))

(use-package auto-virtualenv
  :config
  (setq auto-virtualenv-mode-line nil)
  (add-to-list 'mode-line-misc-info
	       '(auto-virtualenv-mode-line ((:eval (propertize auto-virtualenv-mode-line 'face '(:weight bold :foreground "DeepSkyBlue"))) " ")) t)
  ;; overrides
  (defun auto-virtualenv-update-mode-line ()
    "Update the mode line to show the active virtual environment, or 'N/A' if none."
    (setq auto-virtualenv-mode-line
          (if auto-virtualenv-current-virtualenv
              (format "[Venv: %s]" (file-name-nondirectory
				    (directory-file-name (replace-regexp-in-string "\\.venv/?" "" auto-virtualenv-current-virtualenv))))
	    ""))
    (force-mode-line-update t))
  
  (setq auto-virtualenv-verbose t)
  (setq auto-virtualenv-reload-lsp use-pylsp) ;; need for pylsp. but not need for pyright/basedpyright, it can be auto find .venv
  (auto-virtualenv-setup))

(provide 'init-python)

;;; init-python.el ends here
