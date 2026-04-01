;;; init-z-llm.el --- llm ai setting



;;; Commentary:
;; 

;;; Code:

(use-package agent-shell
  :defer nil
  :load-path "~/.emacs.d/lisp/libs/agent-shell"
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c C-c" . shell-maker-submit)
              ("C-c C-k" . agent-shell-interrupt)
	      ("C-<return>" . find-file-at-point-with-line)
	      ("C-<down-mouse-1>" . find-file-at-point-with-line))

  :config
  
(add-to-list 'display-buffer-alist
	     '("Cursor.*"
	       display-buffer-in-direction
	       (direction . down)))
  )

(provide 'init-z-llm)

;;; init-z-llm.el ends here
