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

  (defun my-agent-shell-dot-subdir (subdir)
    (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
           (sanitized (replace-regexp-in-string "/" ":" cwd)))
      (expand-file-name subdir (concat "~/.cache/emacs/data/agent-shell/" sanitized))))

  (setopt agent-shell-dot-subdir-function #'my-agent-shell-dot-subdir)

  (setq agent-shell-pi-environment
	(agent-shell-make-environment-variables
	 "PI_OFFLINE" "1"
	 "PI_SKIP_VERSION_CHECK" "1"
	 "PI_TELEMETRY" "0"))

  (defalias 'ag> #'agent-shell-send-dwim)
  (add-to-list 'display-buffer-alist
	       '((major-mode . agent-shell-mode)
		 display-buffer-in-direction
		 (direction . down)))

  ;; Support to display directory in buffer listings.
  (add-hook 'agent-shell-mode-hook
	    (lambda ()
	      (setq list-buffers-directory (expand-file-name default-directory))))

  ;;; agent-shell: pi 权限选项按键 + 紧凑标签
  (defcustom my-agent-shell--permission-keys
    '(("\\`No, provide reason"	    . "X")
      ("\\`No\\'"		    . "x")
      ("both directions"            . "b")
      ("reads and writes"           . "b")
      ("\\`Yes, .*for this session" . "!"))
    "Keys for permission options that not match the agent-shell default kind."

    :type '(alist :key-type regexp :value-type string))

  (defun my-agent-shell--quoted-target-to-count (label)
    "Rewrite a single quoted grant target as `1 path'."
    (if (string-match-p "\"[^\"]+\"" label)
	(replace-regexp-in-string "\"[^\"]+\"" "1 path" label t t)
      label))

  (defun my-agent-shell--short-permission-label (name)
    "Compact pi-permission-system's session-option labels."
    (let ((case-fold-search nil))
      (if (string-match "\\`Yes, allow \\(reads and writes\\|reads\\|writes\\|access\\) to .* for this session\\'" name)
          (my-agent-shell--quoted-target-to-count name)
	name)))

  (defun my-agent-shell--permission-advice (orig &rest args)
    "Give every ACP permission option a meaning-based key and a compact label.

pi-acp tags every `select' option `allow_once', so agent-shell hands a
key to the first one only; decide the key from the label instead, and
fall back to agent-shell's own key for any other agent's wording.  Also
unify pi's two label styles (`\"<path>\"' / `N paths') into the latter."
    (let* ((action (apply orig args))
           (opt (plist-get args :acp-option))
           (name (or (map-elt opt 'name) ""))
           (char (or (map-elt action :char)
                     (cdr (seq-find (lambda (rule)
                                      (string-match-p (car rule) name))
                                    my-agent-shell--permission-keys))))
           (label (my-agent-shell--short-permission-label name)))
      (when action
	(when char
          (map-put! action :char char))
        (map-put! action :label (if char (format "%s (%s)" label char) label)))
      action))

  (advice-add 'agent-shell--make-permission-action
              :around #'my-agent-shell--permission-advice)
  )

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

(use-package agent-shell-fork-tree
  :load-path "~/.emacs.d/lisp/libs/agent-shell-fork-tree"
  :hook (agent-shell-mode . agent-shell-fork-tree-mode)
  :bind ("C-c C-f" . agent-shell-fork-tree)

  :custom
  (agent-shell-fork-tree-auto-rebuild nil)
  (agent-shell-fork-tree-message-truncation 80)
  (agent-shell-fork-tree-scan-concurrency 8))

(provide 'init-llm)

;;; init-llm.el ends here
