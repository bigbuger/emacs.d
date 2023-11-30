;;; init-version-control.el --- 版本控制相关配置,如 git、diff


;; magit

;;; Commentary:
;; 

;;; Code:

(require 'magit)
(global-set-key (kbd "C-c g") ' magit-file-dispatch)
(require 'magit-todos)
(magit-todos-mode)

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

(require 'magit-delta)
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
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(require 'hydra)
(defhydra hydra-diff-hl (global-map "C-c v")
  "vc"
  ("n" diff-hl-next-hunk "next hunk")
  ("p" diff-hl-previous-hunk "previous hunk")
  ("r" diff-hl-revert-hunk "revert hunk")
  ("q" nil "exit"))

;; end diff-hl

;; gitlab
(use-package lab :ensure t)

(provide 'init-version-control)

;;; init-version-control.el ends here
