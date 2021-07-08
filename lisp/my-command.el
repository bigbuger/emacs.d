;; -*- coding: utf-8; lexical-binding: t -*-
;;; my-command.el --- My command

;;; Commentary:
;; 

;;; Code:

(defun myselect-line (num)
  "Select NUM lines."
  (interactive "p")
  (progn
    (setq tnum num)
    (forward-line 0)
    (setq startline (line-number-at-pos))
    (set-mark (point))
    (forward-line num)))

(global-set-key (kbd "C-c SPC") 'myselect-line)


(defun mydelete-line (num)
  "Kill NUM lines."
  (interactive "p")
  (save-excursion
    ;; (setq tnum num)
    ;; (forward-line 0)
    ;; (setq startline (line-number-at-pos))
    ;; (set-mark (point))
    ;; (forward-line num)
    (myselect-line num)
    (setq endline (if (= (point) (point-max))
		      (+ (line-number-at-pos) 1)
		    (line-number-at-pos)))
    (kill-region (region-beginning) (region-end))
    (message "kill %d line[s]" (- endline startline))))

(global-set-key (kbd "C-c d") 'mydelete-line)

(defun mycopy-line (num)
  "Copy NUM lines into killring."
  (interactive "p")
  (save-excursion
    ;; (setq tnum num)
    ;; (forward-line 0)
    ;; (setq startline (line-number-at-pos))
    ;; (set-mark (point))
    ;; (forward-line num)
    (myselect-line num)
    (copy-region-as-kill (region-beginning) (region-end))
    (cond ((= (point) (line-beginning-position))
	   (forward-line -1)))
    (setq endline (line-number-at-pos))
    (if (= endline startline)
	(message "Copy line%d into killring" startline)
      (message "Copy line%d to line%d into killring" startline endline))))

(global-set-key (kbd "C-c y") 'mycopy-line)

(defun mycopy-one-word ()
  "Copy one word after or in the point into killring."
  (interactive)
  (save-excursion
    (progn
      (forward-word 1)
      (set-mark (point))
      (backward-word 1)
      (copy-region-as-kill (region-beginning) (region-end)))))
(global-set-key (kbd "C-c M-d") 'mycopy-one-word)

(defun my-sed (sed-cmd)
  "Run the sed commond SED-CMD in current-butter and replace."
  (interactive "ssed:")
  (let ((start (point-min))
	(end (point-max))
	(cmd (concat "sed '" sed-cmd "'" " 2>>/dev/null"))
	(test-cmd (concat "sed '" sed-cmd  "' 2>>/dev/null 1>>/dev/null"))
	(buffer (current-buffer))
	(old-point (point)))
    ;;(save-excursion
    (setq testrun (shell-command-on-region start end test-cmd nil nil))
    (if (= testrun 0)
	(progn
	  (shell-command-on-region start end cmd buffer 1)
	  (goto-char old-point)
	  (message "ok!"))
      (message "sed command :'%s' is error" sed-cmd))));)

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


(defun my-new-line ()
  "Insert new line."
  (interactive)
  (progn
    (move-end-of-line nil)
    (newline-and-indent)))
(global-set-key [(shift return)] 'my-new-line)

(defun my-convert-time (timestamp)
  "Convert TIMESTAMP to iso8601 and put it into kill ring."
  (interactive "ntimestamp:")
  (let ((time-string (format-time-string "%FT%T%z" (seconds-to-time timestamp))))
    (progn
      (kill-new time-string)
      (message time-string))))
(defalias 'ct 'my-convert-time)

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
      (message time-stamp))))
(defalias 'dt 'my-get-timestamp)

(provide 'my-command)

;;; my-command.el ends here
