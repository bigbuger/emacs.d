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
	       `(,rope-python-executable ,rope-cli ,action ,project ,file ,@(mapcar (lambda (arg) (format "%s" arg)) args))
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

(defun rope-extract-variable (var-name)
  "Call rope extract variable for marking region.VAR-NAME is the variable name after extract."
  (interactive "svar name:")
  (save-buffer)
  (when (region-active-p)
    (let* ((start (- (region-beginning) 1))
	   (end (- (region-end) 1)))
      (rope-run-cli-action "extract_variable" start end var-name))))

(defun rope-extract-method (method-name)
  "Call rope extract method for marking region.  METHOD-NAME is the method name after extract."
  (interactive "smethod name:")
  (save-buffer)
  (when (region-active-p)
    (let* ((start (- (region-beginning) 1))
	   (end (- (region-end) 1)))
      (rope-run-cli-action "extract_method" start end method-name))))

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

(defun rope-move (target)
  "Call rope move.  Move thing at point to TARGET file."
  (interactive (list (rope--read-target)))
  (save-buffer)
  (let* ((offset (- (point) 1)))
    (rope-run-cli-action "move" offset target)))

(defun rope-move-module (target)
  "Call rope move module.  Move module to TARGET file."
  (interactive (list (rope--read-target)))
  (save-buffer)
  (rope-run-cli-action "move_module" target))

(require 'transient)
(transient-define-prefix rope-transient ()
  "Rope Refactor action."
  ["Refactor Action"
   ("i" "inlint method"  rope-inline-method)
   ("m" "move thing" rope-move)
   ("M" "move module" rope-move-module)]
  
  [:if region-active-p
   "Extract"
    ("e" "extract variable" rope-extract-variable)
    ("E" "extract variable" rope-extract-method)])

(provide 'python-rope)

;;; python-rope.el ends here
