;;; init-tabbar.el --- tab bar setting
;; centaur-tabs

;;; Commentary:
;; 

;;; Code:


(add-to-list 'load-path "~/.emacs.d/lisp/libs/awesome-tab")
(require 'awesome-tab)

;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)

(setq awesome-tab-height 150)
(when (not (display-graphic-p))
  (setq frame-background-mode light))


(defun awesome-tab-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     ;; just block all buffer start with *
     (and (string-match-p "^[[:space:]]*\\*" name)
	  (not (with-current-buffer x (derived-mode-p 'comint-mode)))
	  (not (with-current-buffer x (derived-mode-p 'compilation-mode)))
	  (not (string-prefix-p "*compilation" name))
	  (not (string-prefix-p "*vterm" name))
	  (not (string-prefix-p "*term" name))
	  (not (string-prefix-p "*shell" name))
	  (not (string-prefix-p "*ielm" name)))

     ;; Is not temp version
     (string-match-p "^.+\\.~.+~$" name)


     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(with-eval-after-load 'projectile
  (defun centaur-tabs-get-group-name-with-perfix (perfix)
    "Return group name start with PERFIX."
    (if (projectile-project-root)
	(concat perfix "#" (awesome-tab-get-group-name (current-buffer)))
      perfix))

  (defun awesome-tab-buffer-groups ()
    "Control buffers' tag group rules."
    (list
     (cond
      ((string-prefix-p org-roam-directory (buffer-file-name))
       "Roam")
      
      ((or (derived-mode-p 'eshell-mode)
	   (derived-mode-p 'term-mode)
	   (derived-mode-p 'shell-mode)
	   (derived-mode-p 'vterm-mode)
	   (derived-mode-p 'compilation-mode)
	   (derived-mode-p 'comint-mode))
       (centaur-tabs-get-group-name-with-perfix "Execute")) ;; 执行模式每个项目一个组

      ((derived-mode-p 'dired-mode)
       (centaur-tabs-get-group-name-with-perfix "Dired")) ;; dired 模式，每个项目一个组

      ;; ((and (not (projectile-project-root))
      ;; 	  (memq major-mode '(org-mode org-agenda-mode diary-mode)))
      ;;  "OrgMode")

      ((or (string-equal "*" (substring (buffer-name) 0 1))
	   (memq major-mode '(magit-process-mode
			      magit-status-mode
			      magit-diff-mode
			      magit-log-mode
			      magit-file-mode
			      magit-blob-mode
			      magit-blame-mode
			      )))
       "Emacs")

      (t
       (awesome-tab-get-group-name (current-buffer)))))))

(add-hook 'after-init-hook
	  (awesome-tab-mode t))

(setq awesome-tab-active-bar-width 1)
(setq awesome-tab-light-selected-foreground-color "#3a81c3")
(setq awesome-tab-light-unselected-foreground-color "#a094a2")
(setq awesome-tab-light-unselected-blend 0.98)
(custom-set-faces
 '(awesome-tab-selected-face ((t (:underline "#3a81c3" :weight bold)))))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
