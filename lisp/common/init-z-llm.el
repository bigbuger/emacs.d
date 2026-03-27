;;; init-z-llm.el --- llm ai setting



;;; Commentary:
;; 

;;; Code:

(use-package agent-shell
  :ensure t

  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c C-c" . shell-maker-submit)
              ("C-c C-k" . agent-shell-interrupt)
	      ("C-<return>" . find-file-at-point-with-line))

  :config
  (defun agent-shell-cursor-make-agent-config ()
    "Create a Cursor agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
    (agent-shell-make-agent-config
     :identifier 'cursor
     :mode-line-name "Cursor"
     :buffer-name "Cursor"
     :shell-prompt "Cursor> "
     :shell-prompt-regexp "Cursor> "
     :icon-name "cursor.png"
     :default-model-id (lambda () "composer-2")
     :welcome-function #'agent-shell-cursor--welcome-message
     :client-maker (lambda (buffer)
                     (agent-shell-cursor-make-client :buffer buffer))
     :install-instructions "Install with: npm install -g @blowmage/cursor-agent-acp\nSee https://github.com/blowmage/cursor-agent-acp-npm for details."))
  )

(add-to-list 'load-path "~/.emacs.d/lisp/libs/agent-review")
(require 'agent-review)

(provide 'init-z-llm)

;;; init-z-llm.el ends here
