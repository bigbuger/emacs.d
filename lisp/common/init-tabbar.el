;;; init-tabbar.el --- tab bar setting
;;

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
  (let* ((name (format "%s" x))
	 (extension (file-name-extension name)))
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
	  (or (not extension)
	      (string= extension "d")))
     )))


(defun centaur-tabs-get-group-name-with-perfix (perfix)
  "Return group name start with PERFIX."
  (let ((project-group (awesome-tab-get-group-name (current-buffer))))
    (if project-group
	(concat perfix "#" project-group)
      perfix)))

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
    ;;	  (memq major-mode '(org-mode org-agenda-mode diary-mode)))
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
     (awesome-tab-get-group-name (current-buffer))))))

(setq awesome-tab-icon-height 0.6)
(setq awesome-tab-icon-file-v-adjust 0)
(setq awesome-tab-height 195)

(defcustom centaur-tabs-close-button (make-string 1 #x00D7)
  "Display appearance of the close buttons, if enabled."
  :group 'centaur-tabs
  :type 'string)

(defun awesome-tab-get-tab-from-event (event)
  "Given a mouse EVENT, extract the tab at the mouse point."
  (let ((pos (posn-string (event-start event))))
    (get-text-property (cdr pos) 'awesome-tabs-tab (car pos))))

(defvar awesome-tabs-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map (vector awesome-tab-display-line 'mouse-1) 'awesome-tab-do-close)
    (define-key map (vector awesome-tab-display-line 'mouse-2) 'awesome-tab-do-close)
    map)
  "Keymap used for setting mouse events for close button.")

(defsubst awesome-tabs-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

;;; Events and event functions
;;
(defun awesome-tabs-buffer-close-tab (tab)
  "Function for closing TAB."
  (let ((buffer (awesome-tabs-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))
    (awesome-tab-refresh-display)))

(defun awesome-tab-do-close (event)
  "Given a mouse EVENT, close the tab at the mouse point."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (with-selected-window window
      (select-window window)
      (let ((foreground-buffer-name (buffer-name)))
	(awesome-tab-buffer-select-tab `,(awesome-tab-get-tab-from-event event))

	(let* ((buffer             (window-buffer window))
	       (target-buffer-name (buffer-name))
	       (same-target-check  (string-equal foreground-buffer-name target-buffer-name))
	       (window-num         (- (length (get-buffer-window-list buffer))
				      (if same-target-check 0 1))))
	  (if (> window-num 1)
	      (delete-window window)
	    (awesome-tabs-buffer-close-tab `,(awesome-tab-get-tab-from-event event))))))))

;; bank the adjust color method.
(defun awesome-tab-adjust-color-with-theme ()
  "We need adjust awesome-tab's colors when user switch new theme."
   (set-face-attribute awesome-tab-display-line nil :height awesome-tab-height))

(defsubst awesome-tab-line-tab (tab)
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
     (propertize (awesome-tab-tab-name tab)
		 'face tab-face
		 'pointer 'hand
		 'local-map (purecopy (awesome-tab-make-header-line-mouse-map
				       'mouse-1
				       `(lambda (event) (interactive "e")
					  (let ((tab-window (window-at (cadr (mouse-position))
								       (cddr (mouse-position))
								       (car (mouse-position)))))
					    (when tab-window
					      (select-window tab-window)
					      (awesome-tab-buffer-select-tab ',tab)))))))

     ;; Close button.
     (propertize
	  centaur-tabs-close-button
	  'face tab-face
	  'pointer 'hand
	  'help-echo "Close buffer"
	  'awesome-tabs-tab tab
	  'local-map awesome-tabs-close-map)

     ;; right margin
     (propertize " " 'face tab-face))))

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
		(all-the-icons-icon-for-mode major-mode :v-adjust awesome-tab-icon-mode-v-adjust :height awesome-tab-icon-height))))))
      (when (and icon
		 ;; `get-text-property' need icon is string type.
		 (stringp icon))
	;; Thanks ema2159 for code block ;)
	(propertize icon 'face `(:inherit ,(get-text-property 0 'face icon) :background ,background :underline ,underline))))))

(define-key awesome-tab-mode-map (kbd "s-t 1")
	    'awesome-tab-kill-other-buffers-in-current-group)
(define-key awesome-tab-mode-map (kbd "s-t g")
	    'awesome-tab-counsel-switch-group)

(add-hook 'after-init-hook
	  (awesome-tab-mode t))


;; hack, ueing awesome-tab-switch-group to let projectile switch to opened project buffer.
(defun smart-switch-project ()
  "Open latest edited buffer when switched the exist project, find files when switched to a new project."
  (let* ((exists-projects (awesome-tab-get-groups))
	 (project-root (projectile-project-root))
	 (tab-group-name (format "Project: %s" project-root)))
    (if (member tab-group-name exists-projects)
	(awesome-tab-switch-group tab-group-name)
      (projectile-find-file))))
(with-eval-after-load 'projectile
  (setq projectile-switch-project-action 'smart-switch-project))

(provide 'init-tabbar)

;;; init-tabbar.el ends here
