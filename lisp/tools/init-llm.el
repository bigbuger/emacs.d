;;; init-z-llm.el --- llm ai setting



;;; Commentary:
;;

;;; Code:

(use-package agent-shell
  :defer nil
  :bind (:map agent-shell-mode-map
              ("RET"              . newline)
	      ("="                . agent-shell-image-scale-increase)
              ("C-c C-c"          . shell-maker-submit)
              ("C-c C-k"          . agent-shell-interrupt)
	      ("C-c C-t"          . agent-shell-open-transcript)
	      ("C-<return>"       . my-find-file-at-point-with-line)
	      ("C-<down-mouse-1>" . my-find-file-at-point-with-line))

  :config
  (setq agent-shell-google-gemini-acp-command '("gemini" "--acp"))
  (setq agent-shell-activity-group-header-label-function
	#'agent-shell-activity-group-tally-label)

  (setopt agent-shell-show-cost-indicator t
	  agent-shell-persistent-prompt-enabled t
	  agent-shell-file-completion-enabled t
	  agent-shell-command-completion-enabled t)

  (defalias 'ag> #'agent-shell-send-dwim)
  (add-to-list 'display-buffer-alist
	       '((major-mode . agent-shell-mode)
		 display-buffer-in-direction
		 (direction . down)))

  ;; Support to display directory in buffer listings.
  (add-hook 'agent-shell-mode-hook
	    (lambda ()
	      (setq list-buffers-directory (expand-file-name default-directory)))))

(use-package agent-shell-ediff
  :load-path "~/.emacs.d/lisp/libs/agent-shell-ediff"
  :after agent-shell
  :custom
  (agent-shell-ediff-quick-quit nil)
  :config
  (agent-shell-ediff-mode 1))

(use-package agent-shell-desktop
  :vc (:url "https://github.com/timfel/agent-shell-desktop.el")
  :after (agent-shell desktop)
  :config
  (agent-shell-desktop-mode 1))

(provide 'init-llm)

;;; init-llm.el ends here
