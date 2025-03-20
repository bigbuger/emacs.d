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

(setq awesome-tab-height 250)

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
	  (not (with-current-buffer x (derived-mode-p 'json-mode)))
	  (not (string-prefix-p "*compilation" name))
	  (not (string-prefix-p "*vterm" name))
	  (not (string-prefix-p "*term" name))
	  (not (string-prefix-p "*shell" name))
	  (not (string-prefix-p "*ielm" name)))

     ;; Is not temp version
     (string-match-p "^.+\\.~.+~$" name)
     (string-prefix-p "xenops" name)
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
    ((or
      (string-prefix-p (file-truename "~/code/go/pkg/mod") (buffer-file-name))
      (string-prefix-p (file-truename "/usr/local/go/src") (buffer-file-name)))
     "Go pkg mod")

    ((or (derived-mode-p 'eshell-mode)
	 (derived-mode-p 'term-mode)
	 (derived-mode-p 'shell-mode)
	 (derived-mode-p 'vterm-mode)
	 (derived-mode-p 'compilation-mode)
	 (derived-mode-p 'comint-mode))
     (centaur-tabs-get-group-name-with-perfix "Execute")) ;; 执行模式每个项目一个组

    ;; ((and (not (projectile-project-root))
    ;;	  (memq major-mode '(org-mode org-agenda-mode diary-mode)))
    ;;  "OrgMode")

    ((or (string-equal "*" (substring (buffer-name) 0 1))
	 (memq major-mode '(magit-process-mode
			    magit-status-mode
			    magit-diff-mode
			    magit-revision-mode
			    magit-log-mode
			    magit-file-mode
			    magit-blob-mode
			    magit-blame-mode
			    )))
     "Emacs")

    (t
     (awesome-tab-get-group-name (current-buffer))))))

(setq awesome-tab-icon-height 0.8)
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

(defvar awesome-tab-nerd-icons-is-load-p (ignore-errors (require 'nerd-icons))
  "Return non-nil if `nerd-icons' is load, `require' will have performance problem, so don't call it dynamically.")


(defsubst awesome-tab-line-tab (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let* ((is-active-tab (awesome-tab-selected-p tab (awesome-tab-current-tabset)))
	 (tab-face (if is-active-tab 'awesome-tab-selected-face 'awesome-tab-unselected-face))
	 (current-buffer-index (cl-position tab (awesome-tab-view (awesome-tab-current-tabset t)))))
    (concat
     ;; left margin
     (propertize " " 'face tab-face)

     ;; Tab icon.
     (when (and awesome-tab-display-icon
		awesome-tab-nerd-icons-is-load-p)
       (awesome-tab-icon-for-tab tab tab-face))

     ;; Tab index.
     (when awesome-tab-show-tab-index
       (propertize (format awesome-tab-index-format-str (+ current-buffer-index 1)) 'face tab-face))
     
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
  "When tab buffer's file is exists, use `nerd-icons-icon-for-file' to fetch file icon.
Otherwise use `nerd-icons-icon-for-buffer' to fetch icon for buffer."
  (when (and awesome-tab-display-icon
	     awesome-tab-nerd-icons-is-load-p)
    (let* ((tab-buffer (car tab))
	   (tab-file (buffer-file-name tab-buffer))
	   (background (face-background face))
	   (underline (face-attribute face :underline))
	   (icon
	    (cond
	     ;; Use `nerd-icons-icon-for-file' if current file is exists.
	     ((and
	       tab-file
	       (file-exists-p tab-file))
	      (nerd-icons-icon-for-file tab-file :v-adjust awesome-tab-icon-file-v-adjust :height awesome-tab-icon-height))
	     ;; Use `nerd-icons-icon-for-mode' for current tab buffer at last.
	     (t
	      (with-current-buffer tab-buffer
		(nerd-icons-icon-for-mode major-mode :v-adjust awesome-tab-icon-mode-v-adjust :height awesome-tab-icon-height))))))
      (when (and icon
		 ;; `get-text-property' need icon is string type.
		 (stringp icon))
	;; Thanks ema2159 for code block ;)
	(propertize icon 'face `(:inherit ,(get-text-property 0 'face icon) :background ,background :underline ,underline))))))

(define-key awesome-tab-mode-map (kbd "s-t 1")
	    'awesome-tab-kill-other-buffers-in-current-group)
(define-key awesome-tab-mode-map (kbd "s-t o")
	    'awesome-tab-switch-group)
(define-key awesome-tab-mode-map (kbd "s-t k")
	    'awesome-tab-kill-all-buffers-in-current-group)
(setq awesome-tab-show-tab-index t)
(setq awesome-tab-index-format-str " %s§")
(define-key awesome-tab-mode-map (kbd "s-1") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-2") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-3") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-4") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-5") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-6") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-7") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-8") 'awesome-tab-select-visible-tab)
(define-key awesome-tab-mode-map (kbd "s-9") 'awesome-tab-select-visible-tab)
;; (define-key awesome-tab-mode-map (kbd "s-0") 'awesome-tab-select-visible-tab)

(add-hook 'after-init-hook
	  (awesome-tab-mode t))


;; hack, ueing awesome-tab-switch-group to let projectile switch to opened project buffer.
(defun smart-switch-project ()
  "Open latest edited buffer when switched the exist project, find files when switched to a new project."
  (let* ((exists-projects (awesome-tab-get-groups))
	 (project-root (projectile-project-root))
	 (tab-group-name (format "Project: %s" project-root)))
    (if (and awesome-tab-mode
	     (member tab-group-name exists-projects))
	(let ((result (awesome-tab-switch-group tab-group-name)))
	  (if (not (buffer-live-p result))
	      (project-find-file)))
      (projectile-find-file))))

(with-eval-after-load 'projectile
  (setq projectile-switch-project-action 'smart-switch-project))

(provide 'init-4-tabbar)

;;; init-4-tabbar.el ends here
