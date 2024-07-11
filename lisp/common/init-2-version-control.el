;;; init-version-control.el --- 版本控制相关配置,如 git、diff


;; magit

;;; Commentary:
;; 

;;; Code:

(require 'magit)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(global-set-key (kbd "C-c c") 'magit-branch-or-checkout)

;; (require 'magit-todos)
;; (magit-todos-mode)
;; ;; magit-todos bug see https://github.com/alphapapa/magit-todos/issues/153
;; (setq magit-todos-nice nil)

(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq magit-git-output-coding-system 'utf-8-unix)
(add-to-list 'magit-process-password-prompt-regexps
	     ".*verification code: ?$")

;; 让magit blame 在左侧边显示
(push '(margin
	(margin-format    . ("%C %a %s%f"))
	(margin-width     . 42)
	(margin-face      . magit-blame-margin)
	(margin-body-face . (magit-blame-dimmed)))
      magit-blame-styles)

;; (require 'magit-delta)
;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;; end magit


;; ediff
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)
(defun ediff-copy-both-to-C ()
  "合并时两边都保留."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; ediff 解决冲突 会显示 4 个面板，源分支、目标分支、合并结果、ancestore，不知道 ancestore 怎么用，就把他给关了
(setq ediff-show-ancestor nil)

;; end ediff

;; diff-hl 侧边显示每一行的版本状态
(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(require 'transient)
(transient-define-prefix diff-hl-transient ()
  "Diff hl."
  ["Jump to git change hunk:"
   [("n" "Next hunk" diff-hl-next-hunk :transient t)]
    [("p" "Previous hunk" diff-hl-previous-hunk :transient t)]
    [("k" "Revert hunk" diff-hl-revert-hunk :transient t)]])
(global-set-key (kbd "C-c v") 'diff-hl-transient)

;; end diff-hl

;; gitlab
(use-package lab
  :ensure t
  :demand t
  
  :config
  
  (cl-defun lab--completing-read-multiple-object (prompt objects &key (formatter #'identity) category predicate require-match initial-input hist def inherit-input-method (sort? t))
    "`completing-read-multiple' with formatter and sort control.
Applies FORMATTER to every object in OBJECTS and propertizes
candidates with the actual object so that they can be retrieved
later by embark actions.  Also adds category metadata to each
candidate, if given.  PROMPT passed to `completing-read-multiple' as is."
    (let* ((object-table
            (make-hash-table :test 'equal :size (length objects)))
           (object-strings
	    (mapcar
             (lambda (object)
               (let ((formatted-object (funcall formatter object)))
		 (puthash formatted-object object object-table)
		 (propertize formatted-object 'lab--completing-read-object object)))
             objects))
           (selecteds
            (completing-read-multiple
             prompt
             object-strings
             predicate require-match initial-input hist def inherit-input-method)))
      (mapcar (lambda (selected) (gethash selected object-table selected)) selecteds)))

  
  (defun lab--create-branch-multiple (projects ref branch)
    (dolist (p projects)
      (let ((project-title (lab--format-project-title p)))
	(condition-case err
	    (progn
	      (lab--request
	       (format "projects/%s/repository/branches" (alist-get 'id p))
	       :branch branch
	       :ref ref
	       :%type "POST"
	       :%raw? t)
	      (message "Create branch %s success: for project %s " branch project-title))
	  (error
	   (message "Create branch %s fail for project %s : %s" branch project-title (error-message-string err)))))))

  (defun lab--read-and-create-branch-multiple (all-projects)
    (let* ((base-prompt "Create branch multiple, ")
	   (projects (lab--completing-read-multiple-object (concat "Create branch for projects: ")
							   all-projects
							   :formatter #'lab--format-project-title
							   :hist 'creat-branch-multiple))
	   (ref (read-string (concat base-prompt "Base branch: ")))
	   (branch (read-string (concat base-prompt "Target branch: "))))
      (lab--create-branch-multiple projects ref branch)))

  (defun lab-create-branch-multiple-for-owned-projects ()
    (interactive)
    (lab--read-and-create-branch-multiple (lab-get-all-owned-projects)))

  (defun lab-create-branch-multiple-for-group-projects ()
    (interactive)
    (lab--read-and-create-branch-multiple (lab-get-all-group-projects)))
  )

(provide 'init-2-version-control)

;;; init-2-version-control.el ends here
