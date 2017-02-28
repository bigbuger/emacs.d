(defun my-insterLine ()
  (interactive)
  (insert-char ?= 60))
(global-set-key (kbd "C-=") 'my-insterLine)


(defun myselect-line (num)
  "select n lines"
  (interactive "p")
  (progn 
    (setq tnum num)
    (forward-line 0)
    (setq startline (line-number-at-pos))
    (set-mark (point))
    (forward-line num)))

(global-set-key (kbd "C-c SPC") 'myselect-line)


(defun mydelete-line (num)
  "kill n lines"
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
  "Copy n lines into killring"
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
  "Run the sed commond in current-butter and replace."
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
