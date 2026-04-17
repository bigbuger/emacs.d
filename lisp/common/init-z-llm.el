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

(provide 'init-z-llm)

;;; init-z-llm.el ends here
