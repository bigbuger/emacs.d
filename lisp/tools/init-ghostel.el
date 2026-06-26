;;; init-ghostel.el --- Ghostel is a terminal emulator for Emacs powered by libghostty-vt, the VT engine behind the Ghostty terminal.
;;; Commentary:
;; 

;;; Code:

(use-package ghostel
  :bind (("C-c t" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-c l"  . consult-line)
         ("C-k"  . my/ghostel-send-C-k-and-kill)
	 ("M-0"  . delete-window)
	 ("M-1"  . delete-other-windows)
	 ("M-2"  . split-window-vertically)
	 ("M-3"  . split-window-horizontally))
  :config
  (setopt ghostel-glyph-scale-floor 1.0)
  
  (defun my/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))
  
  (defun ghostel-projectile (&optional arg)
    "Start a new Ghostel terminal in the current project's root.
The buffer name is prefixed with the project name.
If a buffer already exists for this project, switch to it.
Otherwise create a new Ghostel buffer.  ARG is passed through to
`ghostel' and accepts the same universal argument conventions.
Returns the buffer."
    (interactive "P")
    (let ((default-directory (projectile-acquire-root))
          (ghostel-buffer-name (projectile-generate-process-name "ghostel" arg)))
      (ghostel arg)))
  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "t") 'ghostel-project))
  )

(provide 'init-ghostel)

;;; init-ghostel.el ends here
