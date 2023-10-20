;;; company-plantuml.el --- comany mode for plantuml

;;; Commentary:
;; 

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'plantuml-mode)

(defun company-plantuml--prefix ()
  "Check if is company prefix."
  (and (eq major-mode 'plantuml-mode)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-plantuml--candidates (prefix)
  "Get company condationtes start with PREFIX for plantuml."
  (let ((key-words (hash-table-keys plantuml-kwdList)))
    (cl-remove-if-not
     (lambda (c) (string-prefix-p prefix c))
     key-words)))


;;;###autoload
(defun company-plantuml (command &optional arg &rest ignored)
  "Companybackend for plantuml."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-proto))
    (prefix (company-plantuml--prefix))
    (candidates (company-plantuml--candidates arg))))




(provide 'company-plantuml)

;;; company-plantuml.el ends here
