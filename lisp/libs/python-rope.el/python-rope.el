;;; python-rope.el --- python rope client  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:
(require 'cl-lib)

(defconst rope-cli
  (concat (file-name-directory (or load-file-name buffer-file-name)) "rope_cli.py"))

(defcustom rope-python-executable
  (or (executable-find "python") (executable-find "python3"))
  "Python executable for rope.When using venv, you maybe want to change this."
  :group 'python-rope)

(defcustom rope-project-function
  #'rope--default-project-function
  "Function which returns project root directory."
  :type `(choice
          (const :tag "Default project function" ,#'rope--default-project-function)
          (function :tag "Custom function"))
  :group 'python-rope)

(defun rope--default-project-function ()
  "Return project root directory.
Use projectile if it exits.  Then use build in project.
Otherwise just return `default-directory'."
  (or (when (functionp 'projectile-project-root) (projectile-project-root))
      (when (featurep 'project)
	(when-let* ((proj (project-current)))
	  (project-root proj)))
      default-directory))

(defun rope-run-cli-action (action &rest args)
  "Run the rope cli action."
  (let* ((project (funcall rope-project-function))
	 (file (file-relative-name (buffer-file-name) project))
	 (cmd (string-join
	       `(,rope-python-executable ,rope-cli
					 ,action ,project ,file
					 ,@(mapcar (lambda (arg) (if (stringp arg)
								     (format "\"%s\"" arg)
								   (format "%s" arg)))
						   args))
	       " "))
	 (default-directory project)
	 (proc (start-process-shell-command "rope" "*rope log*" cmd))
	 (sentinel (lambda (proc state)
		     (let ((exit-code (process-exit-status proc))
			   (buffer-file (buffer-file-name)))
		       (if (eq 0 exit-code)
			   (progn
			     (revert-buffer-with-fine-grain t t)
			     (vc-state-refresh buffer-file (vc-backend buffer-file))
			     (when (featurep 'diff-hl)
			       (diff-hl-update))
			     (message "Rope action %s success." action))
			 (error "Rope action %s fail.  see *rope log* for get detail" action))))))
    (set-process-sentinel proc sentinel)))

;;;###autoload
(defun rope-extract-variable (var-name)
  "Call rope extract variable for marking region.VAR-NAME is the variable name after extract."
  (interactive "svar name:")
  (save-buffer)
  (when (region-active-p)
    (let* ((start (- (region-beginning) 1))
	   (end (- (region-end) 1)))
      (rope-run-cli-action "extract_variable" start end var-name))))

;;;###autoload
(defun rope-extract-method (method-name)
  "Call rope extract method for marking region.  METHOD-NAME is the method name after extract."
  (interactive "smethod name:")
  (save-buffer)
  (when (region-active-p)
    (let* ((start (- (region-beginning) 1))
	   (end (- (region-end) 1)))
      (rope-run-cli-action "extract_method" start end method-name))))

;;;###autoload
(defun rope-inline-method ()
  "Call rope inline method.  Inline occurrences of a method."
  (interactive)
  (save-buffer)
  (let* ((offset (- (point) 1)))
    (rope-run-cli-action "inline_method" offset)))

(defun rope--read-target ()
  "Read target file."
  (let ((dir (funcall rope-project-function)))
    (file-relative-name (read-file-name "target: " nil nil t) dir)))

;;;###autoload
(defun rope-move (target)
  "Call rope move.  Move thing at point to TARGET file."
  (interactive (list (rope--read-target)))
  (save-buffer)
  (let* ((offset (- (point) 1)))
    (rope-run-cli-action "move" offset target)))

;;;###autoload
(defun rope-move-module (target)
  "Call rope move module.  Move module to TARGET file."
  (interactive (list (rope--read-target)))
  (save-buffer)
  (rope-run-cli-action "move_module" target))

(defun rope--get-parameter-name-of-node (node)
  "Find node's paremter-name."
  (let* ((typed-node (treesit-parent-until
		      node
		      (lambda (p)
			(string-equal "typed_parameter"
				      (treesit-node-type p)))
		      t))
	 (ident-node (if typed-node
			 (treesit-node-child typed-node 0)
		       node)))
    (when ident-node
      (treesit-node-text ident-node t))))

(defun rope--find-parameter-index ()
  "Find current point's parameter index of def function.
Need treesit."
  (when (derived-mode-p 'python-ts-mode)
    (let* ((node-at-point (treesit-node-at (point)))
	   (node (if (string-equal "," (treesit-node-type node-at-point))
		     (treesit-node-next-sibling node-at-point)
		   node-at-point))
	   (parameter-name (rope--get-parameter-name-of-node node))
	   (parameters (treesit-parent-until
			node
			(lambda (p)
			  (string-equal "parameters"
					(treesit-node-type p)))))
	   (parameter-nodes (when parameters
			      (treesit-filter-child parameters
						    (lambda (c)
						      (member (treesit-node-type c) '("typed_parameter" "identifier"))))))
	   index)
      (when parameter-nodes
	(cl-position-if (lambda (n) (string-equal (rope--get-parameter-name-of-node n) parameter-name))
			parameter-nodes)))))

(defun rope--find-function-name-and-offset ()
  (let* ((function-node (treesit-parent-until
			 (treesit-node-at (point))
			 (lambda (n)
			   (string-equal "function_definition"
					 (treesit-node-type n)))))
	 (name-node (when function-node
		      (treesit-node-child-by-field-name function-node "name"))))
    (when name-node
      (list
       (treesit-node-text name-node)
       (- (treesit-node-start name-node) 1)))))

;;;###autoload
(defun rope-add-parameter (index)
  "Call rope add parameter.
If INDEX has value，add parameter at this index, else add by index at current point,
otherwise if current point not at parameters, add at end.

WARNING: rope will make all type hint gone!"
  (interactive "P")
  (let* ((index (or index
		    (rope--find-parameter-index)
		    -1))
	 (function-name-and-offset (rope--find-function-name-and-offset))
	 (function-name (car function-name-and-offset))
	 (offset (cadr function-name-and-offset))
	 (parameter (read-string (format "Add parameter for %s at index %s: " function-name index)))
	 (name-and-default (split-string parameter "=" t " "))
	 (name (car name-and-default))
	 (default (or (cadr name-and-default) "")))
    (rope-run-cli-action "argument_add" offset index name default)))

;;;###autoload
(defun rope-remove-parameter (index)
  "Call rope remove parameter.
If INDEX has value，remove parameter at this index, else remove by index at current point.

WARNING: rope will make all type hint gone!"
  (interactive "P")
  (let* ((index (or index
		    (rope--find-parameter-index)))
	 (function-name-and-offset (rope--find-function-name-and-offset))
	 (function-name (car function-name-and-offset))
	 (offset (cadr function-name-and-offset)))
    (when index
      (rope-run-cli-action "argument_remove" offset index))))

(require 'transient)
(transient-define-prefix rope-transient ()
  "Rope Refactor action."
  ["Refactor Action"
   ("i" "inlint method"  rope-inline-method)
   ("m" "move thing" rope-move)
   ("M" "move module" rope-move-module)]
  
  [:if region-active-p
       "Extract"
       ("ev" "extract variable" rope-extract-variable)
       ("em" "extract method" rope-extract-method)]

  [:if (lambda () (derived-mode-p 'python-ts-mode))
       "Change signature"
       ("pa" "add parameter" rope-add-parameter)
       ("pr" "remove parameter" rope-remove-parameter)])

(provide 'python-rope)

;;; python-rope.el ends here
