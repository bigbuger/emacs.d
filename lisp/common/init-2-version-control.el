;;; init-version-control.el --- 版本控制相关配置,如 git、diff


;; magit

;;; Commentary:
;; 

;;; Code:

(require 'magit)
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(unbind-key "M-1" 'magit-section-mode-map)
(unbind-key "M-2" 'magit-section-mode-map)
(unbind-key "M-3" 'magit-section-mode-map)
(unbind-key "M-4" 'magit-section-mode-map)

(setq magit-delete-by-moving-to-trash nil)

(setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq magit-status-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
(setq magit-git-output-coding-system 'utf-8-unix)
(add-to-list 'magit-process-password-prompt-regexps
	     ".*verification code: ?$")

;; 让magit blame 在左侧边显示
(push '(margin
	(margin-format    . ("%C %a"))
	(margin-width     . 30)
	(margin-face      . magit-blame-margin)
	(margin-body-face . (magit-blame-dimmed)))
      magit-blame-styles)

(defun magit-blame-copy-abbrev-hash ()
  "Save hash of the current chunk's commit to the kill ring.

When the region is active, then save the region's content
instead of the hash, like `kill-ring-save' would."
  (interactive)
  (if (use-region-p)
      (call-interactively #'copy-region-as-kill)
    (kill-new (message "%s"
		       (substring  (oref (magit-current-blame-chunk) orig-rev)
				   0
				   (or magit-blame--abbrev-length
				       (setq magit-blame--abbrev-length
					     (magit-abbrev-length))))))))

(define-key magit-blame-read-only-mode-map
	    (kbd "M-w") #'magit-blame-copy-abbrev-hash)

(defvar magit-read-file-multiple-hist nil)

(defun magit-read-file-from-rev-multiple (rev prompt &optional default include-dirs)
  (let ((files (magit-revision-files rev)))
    (when include-dirs
      (setq files (sort (nconc files (magit-revision-directories rev))
                        #'string<)))
    (magit-completing-read-multiple
     prompt files nil t nil 'magit-read-file-multiple-hist
     (car (member (or default (magit-current-file)) files)))))

(defvar magit-file-checkout-multiple--rev nil)

(defun magit-file-checkout-multiple--run (files)
  (magit-with-toplevel
    (magit-run-git "checkout" magit-file-checkout-multiple--rev "--" files)))

(defvar-keymap embark-magit-file-checkout-multiple-actions
    :doc "Keymap for actions for lsp-identifier."
    :parent embark-general-map
    "c" #'magit-file-checkout-multiple--run)

(defun magit-file-checkout-multiple (files)
  "Checkout FILE from REV."
  (interactive
   (let ((rev (magit-read-branch-or-commit
               "Checkout from revision" magit-buffer-revision)))
     (setq magit-file-checkout-multiple--rev rev)
     (list (minibuffer-with-setup-hook
	       (lambda ()
		 (when (featurep 'embark)
		   (setq-local embark-keymap-alist
			       '((t embark-magit-file-checkout-multiple-actions)))))
	     (magit-read-file-from-rev-multiple rev "Checkout file" nil t)))))
  (magit-file-checkout-multiple--run files))

(transient-replace-suffix 'magit-reset (kbd "f")
  '("f" "Files" magit-file-checkout-multiple))

(with-eval-after-load 'embark
   (add-to-list 'embark-multitarget-actions 'magit-file-checkout-multiple--run))

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

;; -d --minimal
;; -B --ignore-blank-lines
(setopt ediff-diff-options "-dB")

;; end ediff

(use-package difftastic-bindings
  :ensure difftastic ;; or nil if you prefer manual installation
  :init
  (use-package transient               ; to silence compiler warnings
    :autoload (transient-get-suffix
               transient-parse-suffix))

  (use-package magit-blame
    :defer t :ensure magit
    :bind
    (:map magit-blame-read-only-mode-map
          ("M-RET" . #'difftastic-magit-show))
    :config
    (let ((suffix '("M-RET" "Difftastic show" difftastic-magit-show)))
      (unless (equal (transient-parse-suffix 'magit-blame suffix)
                     (transient-get-suffix 'magit-blame "b"))
        (transient-append-suffix 'magit-blame "b" suffix)))
    (use-package magit-diff
      :defer t :ensure magit
      :config
      (let ((suffix [("M-d" "Difftastic diff (dwim)" difftastic-magit-diff)
                     ("M-c" "Difftastic show" difftastic-magit-show)]))
        (unless (equal (transient-parse-suffix 'magit-diff suffix)
                       (transient-get-suffix 'magit-diff '(-1 -1)))
          (transient-append-suffix 'magit-diff '(-1 -1) suffix))))))

;; diff-hl 侧边显示每一行的版本状态
(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
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
      (let ((project-title (lab--format-project-title-simple p)))
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

  (defun lab--format-project-title-simple (project)
    (alist-get 'path_with_namespace project))
  
  (defun lab--read-and-create-branch-multiple (all-projects)
    (let* ((base-prompt "Create branch multiple, ")
	   (projects (lab--completing-read-multiple-object (concat "Create branch for projects: ")
							   all-projects
							   :formatter #'lab--format-project-title-simple
							   :hist 'creat-branch-multiple)))
      (lab--read-and-create-branch-multiple-run projects)))

  (defun lab--read-and-create-branch-multiple-run (projects)
    (let* ((base-prompt "Create branch multiple, ")
	   (ref (read-string (concat base-prompt "Base branch: ")))
	   (branch (read-string (concat base-prompt "Target branch: "))))
      (lab--create-branch-multiple projects ref branch)))
   
  (defun lab-create-branch-multiple-for-owned-projects ()
    (interactive)
    (lab--read-and-create-branch-multiple (lab-get-all-owned-projects)))

  (defun lab-create-branch-multiple-for-group-projects ()
    (interactive)
    (lab--read-and-create-branch-multiple (lab-get-all-group-projects)))

  (with-eval-after-load 'embark
    (setf (alist-get 'lab-create-branch-multiple-for-owned-projects embark-default-action-overrides) #'lab--read-and-create-branch-multiple-run
	  (alist-get 'lab-create-branch-multiple-for-group-projects embark-default-action-overrides) #'lab--read-and-create-branch-multiple-run)
    (add-to-list 'embark-multitarget-actions #'lab--read-and-create-branch-multiple-run)) ; FIXME no work 
  )
  

(provide 'init-2-version-control)

;;; init-2-version-control.el ends here
