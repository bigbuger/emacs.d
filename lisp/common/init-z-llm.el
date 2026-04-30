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
	      ("C-c C-t" . agent-shell-open-transcript)
	      ("C-<return>" . find-file-at-point-with-line)
	      ("C-<down-mouse-1>" . find-file-at-point-with-line))

  :config
  (setq agent-shell-google-gemini-acp-command '("gemini" "--acp"))
  
  (defalias 'ag> #'agent-shell-send-dwim)
  (add-to-list 'display-buffer-alist
	       '((major-mode . agent-shell-mode)
		 display-buffer-in-direction
		 (direction . down)))

  (defun agent-shell-latex-section (section)
    (when (and (map-nested-elt section '(:body :start))
	       (map-nested-elt section '(:body :end)))
      (require 'org)
      ;; Silence org-element warnings (hacky!!)
      (let ((major-mode 'org-mode))
        (save-excursion
          (save-restriction
            (narrow-to-region (map-nested-elt section '(:body :start))
                              (map-nested-elt section '(:body :end)))
            (org-format-latex
             (concat org-preview-latex-image-directory "markdown-overlays")
             (point-min) (point-max)
             temporary-file-directory
             'overlays nil 'forbuffer org-preview-latex-default-process))))))

  ;; too slow
  ;; (add-hook 'agent-shell-section-functions
  ;;           #'agent-shell-latex-section
  ;;           nil nil)
  )

(use-package agent-shell-macext
  :vc (:url "https://github.com/cxa/agent-shell-macext")
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom
  (agent-shell-macext-file-copy-policy 'auto)    ; auto, always-copy, always-original
  (agent-shell-macext-notifications t)           ; enable native notifications
  (agent-shell-macext-notify-current-buffer nil)) ; nil = suppress when shell/viewport is current and Emacs is focused

(provide 'init-z-llm)

;;; init-z-llm.el ends here
