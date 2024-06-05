;;; consult-dasel.el --- Lieve preview of "dasel" queries using consult  -*- lexical-binding: t; -*-

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
;;;   Needs the "dasel" binary installed.

;;; Code:

(require 'consult)

(defcustom consult-dasel-toml-buffer-mode 'conf-toml-mode
  "Major mode for the resulting `consult-dasel-buffer' buffer."
  :type '(function)
  :require 'consult-dasel
  :group 'consult-dasel)

(defcustom consult-dasel-command "dasel"
  "Command for `consult-dasel'."
  :type '(string)
  :require 'consult-dasel
  :group 'consult-dasel)


(defcustom consult-dasel-jq-path-query
  "[ path(..) | map(select(type == \"string\") // \"[]\") | join(\".\") ] | sort | unique | .[] | split(\".[]\") | join(\"[]\") | \".\" + ."
  "Use dasel to get all json path."
  :type '(string)
  :require 'consult-dasel
  :group 'consult-dasel)

(defcustom consult-dasel-buffer "*consult-dasel-result*"
  "Buffer for the `consult-dasel' query results."
  :type '(string)
  :require 'consult-dasel
  :group 'consult-dasel)

(add-to-list 'display-buffer-alist
	     `(,consult-dasel-buffer
	       display-buffer-in-direction
	       (direction . right)))


(defun consult-dasel-path (buffer)
  "Get all path in BUFFER."
  (with-current-buffer buffer
    (split-string
     (with-output-to-string
       (call-shell-region
	(point-min)
	(point-max)
	(concat consult-dasel-command " -r toml -w json " " | jq -r '"  consult-dasel-jq-path-query "'")
	nil
	standard-output))
     "\n")))

(defun consult-dasel-query (buffer input)
  "Call dasel whith current buffer `BUFFER', query `INPUT'."
  (when (get-buffer consult-dasel-buffer)
      (with-current-buffer consult-dasel-buffer
        (funcall consult-dasel-toml-buffer-mode)
        (erase-buffer)))
  (with-current-buffer buffer
    (call-process-region
     (point-min)
     (point-max)
     consult-dasel-command
     nil
     consult-dasel-buffer
     nil
     "-rtoml"
     (or input ".")))
  (list (with-current-buffer consult-dasel-buffer
	  (buffer-string))))


(defun consult-dasel-state-builder (buffer)
  "Build STATE of consult-dasel."
  (lambda (_act cand)
    (consult-dasel-query buffer cand)
    (display-buffer consult-dasel-buffer)))

(defun consult-dasel-read-dasel (buffer)
  "Read input and run dasel."
  (interactive)
  (let ((canditdates (consult-dasel-path buffer))
	(state (consult-dasel-state-builder buffer))
	(map (make-sparse-keymap)))
    (consult--read
     canditdates
     :prompt "dasel: "
     :initial "."
     :sort nil
     :state state
     :keymap map)))

;;;###autoload
(defun consult-dasel ()
  "Consult interface for dynamically querying dasel.
Whenever you're happy with the query, hit RET and the results
will be displayed to you in the buffer in `consult-dasel-buffer'."
  (interactive)
  (let ((buffer (current-buffer)))
    (consult-dasel-read-dasel buffer)))

(provide 'consult-dasel)

;;; consult-dasel.el ends here

