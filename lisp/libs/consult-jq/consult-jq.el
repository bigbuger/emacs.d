;;; consult-jq.el --- Lieve preview of "jq" queries using consult  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  bigbuger

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;   Needs the "jq" binary installed.

;;; Code:

(require 'consult)

(defcustom consult-jq-json-buffer-mode 'js-mode
  "Major mode for the resulting `consult-jq-buffer' buffer."
  :type '(function)
  :require 'consult-jq
  :group 'consult-jq)

(defcustom consult-jq-command "jq"
  "Command for `consult-jq'."
  :type '(string)
  :require 'consult-jq
  :group 'consult-jq)

(defcustom consult-jq-path-query
  "[ path(..) | map(select(type == \"string\") // \"[]\") | join(\".\") ] | sort | unique | .[] | split(\".[]\") | join(\"[]\") | \".\" + ."
  "Use jq to get all json path."
  :type '(string)
  :require 'consult-jq
  :group 'consult-jq)

(defcustom consult-jq-buffer "*consult-jq-result*"
  "Buffer for the `consult-jq' query results."
  :type '(string)
  :require 'consult-jq
  :group 'consult-jq)

(add-to-list 'display-buffer-alist
	     `(,consult-jq-buffer
	       display-buffer-in-direction
	       (direction . right)))

(defun consult-jq-get-custom-function ()
  "Retrun all jq custom function which define in .jq file."
  (let ((jq-file (file-truename "~/.jq"))
	(matches))
    (if (file-readable-p jq-file)
	(with-temp-buffer
	  (insert-file-contents jq-file)
	  (goto-char (point-min))
	  (save-match-data
	    (goto-char 1)
            (while (search-forward-regexp "def *\\([a-zA-Z_]+\\)" nil t 1)
              (push (match-string 1) matches)))
	  matches))))
  
(defun consult-jq-get-all-jq-function ()
  "Retrun all jq function."
  (let ((builtins (mapcar (lambda (c) (car (split-string c "/")))
			  (string-split (shell-command-to-string "echo '{}' | jq -r 'builtins | .[]'"))))
	(customs (consult-jq-get-custom-function)))
    (append builtins customs)))

(defun make-consult-jq-completion-function-at-point (buffer)
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (lambda ()
    (let* ((text (thing-at-point 'symbol))
	   (contents (minibuffer-contents-no-properties))
	   (bds (bounds-of-thing-at-point 'symbol))
	   (needpath-p (string-suffix-p (concat "." text) contents))  ;; need to complete json path if end of "."
           (start (if needpath-p
		      (- (car bds) 1)
		    (car bds)))
           (end (cdr bds))
	   (pip-index (string-match-p (regexp-quote "|") contents))
	   (query (when pip-index (substring contents 0 pip-index)))
	   (paths (when needpath-p
		    (cl-remove-if #'string-blank-p
				  (consult-jq-path buffer query)))))
      (list start end (or paths (consult-jq-get-all-jq-function)) . nil ))))

(defun consult-jq-call-jq (&optional query args output-buffer)
  "Call 'jq' use OUTPUT-BUFFER as output (default is 'standard-output').
with the QUERY and ARGS."
  (call-process-region
   (point-min)
   (point-max)
   consult-jq-command
   nil
   (or output-buffer standard-output)
   nil
   (or args  "-M")
   (or query ".")))


(defun consult-jq-json (buffer &optional query)
  "Call 'jq' under BUFFER with the QUERY with a default of '.'."
  (with-current-buffer
      buffer
    (consult-jq-call-jq query nil consult-jq-buffer)))

(defun consult-jq-path (buffer &optional query)
  "Get all json path, after apply QUERY in BUFFER."
  (with-current-buffer buffer
    (split-string
     (with-output-to-string
       (consult-jq-call-jq (concat (or query "." ) " | " consult-jq-path-query)
			   "-r"))
     "\n")))


(defun consult-jq-query-function (buffer input)
  "Wrapper function passing INPUT over to `consult-jq-json'."
  (when (get-buffer consult-jq-buffer)
      (with-current-buffer consult-jq-buffer
        (funcall consult-jq-json-buffer-mode)
        (erase-buffer)))
  (consult-jq-json buffer input)
  (list (with-current-buffer consult-jq-buffer
	  (buffer-string))))


(defun consult-jq-state-builder (buffer)
  "Build STATE of consult-jq."
  (lambda (_act cand)
    (consult-jq-query-function buffer cand)
    (display-buffer consult-jq-buffer)))

(defun consult-jq-read-jq (buffer)
  "Read input and run jq."
  (interactive)
  (let ((canditdates (consult-jq-path buffer))
	(state (consult-jq-state-builder buffer)))
    (minibuffer-with-setup-hook
	(:append
	 (lambda ()
	   (add-hook 'completion-at-point-functions
		     (make-consult-jq-completion-function-at-point buffer) nil t)))
      (consult--read
       canditdates
       :prompt "jq: "
       :initial "."
       :sort nil
       :state state))))

;;;###autoload
(defun consult-jq ()
  "Consult interface for dynamically querying jq.
Whenever you're happy with the query, hit RET and the results
will be displayed to you in the buffer in `consult-jq-buffer'."
  (interactive)
  (let ((buffer (current-buffer)))
    (consult-jq-read-jq buffer)))

(provide 'consult-jq)

;;; consult-jq.el ends here
