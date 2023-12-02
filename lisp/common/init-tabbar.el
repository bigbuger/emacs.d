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

(setq awesome-tab-icon-height 0.6)
(setq awesome-tab-icon-file-v-adjust 0)
(setq awesome-tab-height 195)

;; bank the adjust color method.
(defun awesome-tab-adjust-color-with-theme ()
  "We need adjust awesome-tab's colors when user switch new theme."
   (set-face-attribute awesome-tab-display-line nil :height awesome-tab-height))

(defun awesome-tab-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let* ((is-active-tab (awesome-tab-selected-p tab (awesome-tab-current-tabset)))
         (tab-face (if is-active-tab 'awesome-tab-selected-face 'awesome-tab-unselected-face)))
    (concat
     ;; left margin
     (propertize " " 'face tab-face)
     ;; Tab icon.
     (when (and awesome-tab-display-icon
                awesome-tab-all-the-icons-is-load-p)
       (awesome-tab-icon-for-tab tab tab-face))
     ;; Tab label.
     (propertize (awesome-tab-tab-name tab) 'face tab-face))))

(defun awesome-tab-icon-for-tab (tab face)
  "When tab buffer's file is exists, use `all-the-icons-icon-for-file' to fetch file icon.
Otherwise use `all-the-icons-icon-for-buffer' to fetch icon for buffer."
  (when (and awesome-tab-display-icon
             awesome-tab-all-the-icons-is-load-p)
    (let* ((tab-buffer (car tab))
           (tab-file (buffer-file-name tab-buffer))
           (background (face-background face))
	   (underline (face-attribute face :underline))
           (icon
            (cond
             ;; Use `all-the-icons-icon-for-file' if current file is exists.
             ((and
               tab-file
               (file-exists-p tab-file))
              (all-the-icons-icon-for-file tab-file :v-adjust awesome-tab-icon-file-v-adjust :height awesome-tab-icon-height))
             ;; Use `all-the-icons-icon-for-mode' for current tab buffer at last.
             (t
              (with-current-buffer tab-buffer
                (if (derived-mode-p tab-buffer 'eaf-mode)
                    (all-the-icons-faicon "html5"  :v-adjust awesome-tab-icon-mode-v-adjust :height awesome-tab-icon-height)
                  (all-the-icons-icon-for-mode major-mode :v-adjust awesome-tab-icon-mode-v-adjust :height awesome-tab-icon-height))
                )))))
      (when (and icon
                 ;; `get-text-property' need icon is string type.
                 (stringp icon))
        ;; Thanks ema2159 for code block ;)
        (propertize icon 'face `(:inherit ,(get-text-property 0 'face icon) :background ,background :underline ,underline))))))


(add-hook 'after-init-hook
	  (awesome-tab-mode t))


(provide 'init-tabbar)

;;; init-tabbar.el ends here
