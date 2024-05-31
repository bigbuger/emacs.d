;;; company-go-tag.el --- company backend for go tag

;;; Commentary:
;; 


;;; Code:


(require 'cl-lib)
(require 'company)
(require 'treesit)

(defcustom company-go-tag-alist
  '(("json" . ("omitempty"
	     "string"))
    ("form"))
  "Company go tag alist."
  :type 'list
  :group 'company-go-tag)

(defun company-go-tag-field-name ()
  (let* ((node (treesit-node-at (point)))
	 (node-type (treesit-node-type node))
	 (node-parent (treesit-node-parent node))
	 (node-parent-type (treesit-node-type node-parent)))
    (if (string-equal "field_declaration" node-parent-type)
	(let ((identifier (treesit-node-child node-parent 0)))
	  (treesit-node-text identifier)))))


(defun company-go-tag--prefix ()
  "Check if is company prefix."
  (if (eq major-mode 'go-ts-mode)
      (let* ((node (treesit-node-at (point)))
	     (node-type (treesit-node-type node))
	     (node-parent (treesit-node-parent node))
	     (node-parent-type (treesit-node-type node-parent)))
	(and (string-equal "raw_string_literal" node-type)
	     (string-equal "field_declaration" node-parent-type)
	     (company-grab-symbol)))))

(defun company-go-tag--candidates (prefix)
  (let* ((node (treesit-node-at (point)))
	 (k (when (save-excursion
		    (re-search-backward "[` ,\"]\\(.+\\):?\""
		     (treesit-node-start node) t 1))
	      (match-string 1)))
	 (scope (when k (string-replace ":" "" k)))
	 (candidates (cdr (assoc scope company-go-tag-alist))))
    (cl-remove-if-not (lambda (c) (string-prefix-p prefix c))
		      (if (not (string-suffix-p ":" k))
			  (mapcar #'car company-go-tag-alist)
			;; TODO support function
			candidates))))


;;;###autoload
(defun company-go-tag (command &optional arg &rest ignored)
  "Companybackend for protobuf."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-go-tag))
    (prefix (company-go-tag--prefix))
    (candidates (company-go-tag--candidates arg))))


(provide 'company-go-tag)

;;; company-go-tag.el ends here
