;; -*- coding: utf-8; lexical-binding: t -*-
;;; my-command.el --- My command

;;; Commentary:
;; 

;;; Code:

(defun my-select-line (num)
  "Select NUM lines."
  (interactive "p")
  (let (startline)
    (progn
      (forward-line 0)
      (setq startline (line-number-at-pos))
      (set-mark (point))
      (end-of-line num)
      startline)))

(global-set-key (kbd "C-c SPC") 'my-select-line)

(defun my-delete-line (num)
  "Kill NUM lines."
  (interactive "p")
  (save-excursion
    (progn
      (beginning-of-line)
      (kill-line num))))

(global-set-key (kbd "C-c d") 'my-delete-line)

(defun my-copy-line (num)
  "Copy NUM lines into killring."
  (interactive "p")
  (save-excursion
    (let  ((startline (my-select-line num))
	   endline)
      (copy-region-as-kill (region-beginning) (region-end))
      (cond ((= (point) (line-beginning-position))
	     (forward-line -1)))
      (setq endline (line-number-at-pos))
      (if (= endline startline)
	  (message "Copy line %d into killring" startline)
	(message "Copy line %d to line %d into killring" startline endline)))))

(global-set-key (kbd "C-c y") 'my-copy-line)

(defun my-sed (sed-cmd)
  "Run the sed commond SED-CMD in current-butter and replace."
  (interactive "ssed:")
  (let* ((start (point-min))
	 (end (point-max))
	 (cmd (concat "sed '" sed-cmd "'" " 2>>/dev/null"))
	 (test-cmd (concat "sed '" sed-cmd  "' 2>>/dev/null 1>>/dev/null"))
	 (buffer (current-buffer))
	 (old-point (point))
	 (testrun (shell-command-on-region start end test-cmd nil nil)))
    (if (= testrun 0)
	(progn
	  (shell-command-on-region start end cmd buffer 1)
	  (goto-char old-point)
	  (message "ok!"))
      (message "sed command :'%s' is error" sed-cmd))))

(global-set-key (kbd "C-c :") 'my-sed)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

;;(global-set-key (kbd "C-d") 'duplicate-line)

(defun my-indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(global-set-key (kbd "C-M-l") 'my-indent-buffer)


(defun my-new-line ()
  "Insert new line."
  (interactive)
  (progn
    (move-end-of-line nil)
    (newline-and-indent)))
(global-set-key [(shift return)] 'my-new-line)

(require 'subword)
(defmacro def-subword-action (fun action)
  `(defun ,fun (arg)
     (interactive "p")
     (let ((subword-mode 1)
	   (find-word-boundary-function-table subword-find-word-boundary-function-table))
       (,action arg))))

(def-subword-action my-backward-kill-subword backward-kill-word)
(def-subword-action my-left-subword left-word)
(def-subword-action my-right-subword right-word)

(global-set-key (kbd "M-s-<backspace>") #'my-backward-kill-subword)
(global-set-key (kbd "M-s-<left>") #'my-left-subword)
(global-set-key (kbd "M-s-<right>") #'my-right-subword)


(defun my-convert-time (timestamp)
  "Convert TIMESTAMP to iso8601 and put it into kill ring."
  (interactive "ntimestamp:")
  (let ((time-string (format-time-string "%FT%T%z" (seconds-to-time timestamp))))
    (progn
      (kill-new time-string)
      (message "date is: %s" time-string))))
(defalias 'd2t 'my-convert-time)

(defun my-get-timestamp (time-string)
  "Convert TIME-STRING to unix timestamp and put it into kill ring."
  (interactive "stime-string:")
  (let* ((tl0 (parse-time-string time-string))
	 (tl-with-out-zone (butlast tl0))
	 (tl (append (mapcar #'(lambda (i) (if i i 0)) tl-with-out-zone)
		     (last tl0)))
	 (et (apply #'encode-time tl))
	(time-stamp (format-time-string "%s" et)))
    (progn
      (kill-new time-stamp)
      (message "timestamp is: %s" time-stamp))))
(defalias 't2d 'my-get-timestamp)

(provide 'my-command)

;;; my-command.el ends here
