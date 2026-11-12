;; -*- coding: utf-8; lexical-binding: t -*-
;;; my-edit-command.el --- Add on some based commands, make some help key bind to make edit simplify.

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

(global-set-key (kbd "M-S-<backspace>") 'my-copy-line)

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
(def-subword-action my-left-subword          left-word)
(def-subword-action my-right-subword         right-word)

(global-set-key (kbd "M-s-<backspace>") #'my-backward-kill-subword)
(global-set-key (kbd "M-s-<left>")      #'my-left-subword)
(global-set-key (kbd "M-s-b")           #'my-left-subword)
(global-set-key (kbd "M-s-<right>")     #'my-right-subword)
(global-set-key (kbd "M-s-f")           #'my-right-subword)

;; 跳转文件时支持走到后面的行号
(defun my-find-file-at-point-with-line(prefix)
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive "p")
  (let ((line-num 0))
    (save-excursion
      (unless (search-forward-regexp "[^ ]:" (line-end-position) t)
	(when (and
	       (search-backward-regexp "[^0-9][0-9]+:[0-9]+:" (line-beginning-position) t)
	       (not (looking-at "[0-9]")))
	  (forward-char 1)))
      (if (looking-at "[0-9]+")
          (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
    (if (> prefix 1)
	(find-file (ffap-guesser))
      (find-file-other-window (ffap-guesser)))
    (if (not (equal line-num 0))
	(progn
	  (goto-char (point-min))
	  (forward-line (- line-num 1))))))

(global-set-key (kbd "C-<return>") #'my-find-file-at-point-with-line)

(provide 'my-edit-command)

;;; my-command.el ends here
