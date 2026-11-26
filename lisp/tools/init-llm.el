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
	      ("C-M-p"            . comint-previous-prompt)
	      ("C-M-n"            . comint-next-prompt)
	      ("C-<return>"       . my-find-file-at-point-with-line)
	      ("C-<down-mouse-1>" . my-find-file-at-point-with-line))

  :config
  (setq agent-shell-google-gemini-acp-command '("gemini" "--acp"))
  (setq agent-shell-activity-group-header-label-function
	#'agent-shell-activity-group-tally-label)
  (setq agent-shell-session-restore-verbosity 'full)
  (setq agent-shell-prompt-busy-frames '("▶▷▷" "▷▶▷" "▷▷▶")
	agent-shell-chat--body-indent "   "
	agent-shell-elicitation--indent "    ")

  (setopt agent-shell-show-cost-indicator t
	  agent-shell-persistent-prompt-enabled t
	  agent-shell-file-completion-enabled t
	  agent-shell-command-completion-enabled t)

  (defun my-agent-shell-imenu-index ()
    "Imenu index of agent-shell turns, from prompt text properties."
    (let (entries)
      (dolist (range (agent-shell-prompt-ranges))
	(let* ((beg (map-elt range :input-start))
               (end (min (map-elt range :input-end) (point-max)))
               (text (string-replace "\n" " ⏎ " (string-trim (buffer-substring-no-properties beg end)))))
          (unless (string-empty-p text)
            (push (cons text
                        (copy-marker beg))
                  entries))))
      (nreverse entries)))

  (add-hook 'agent-shell-mode-hook
            (lambda ()
              (setq-local imenu-create-index-function
                          #'my-agent-shell-imenu-index)))


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

  ;;; 权限选项按键，有些 agent (如 pi) 没有返回 kind，另外默认支持的 kind 不多
  (defcustom my-agent-shell--permission-keys
    '(("\\`No, provide reason"	      . "X")
      ("\\`No\\'"		      . "x")
      ("both directions"              . "b")
      ("reads and writes"             . "b")
      ("\\`Yes, .*for this session"   . "!")
      ("\\`Implement here"            . "i")
      ("\\`Start fresh and implement" . "I")
      ("\\`Export plan"               . "e")
      ("\\`Save for later"            . "l")
      ("\\`Stay in Plan mode"         . "s")

      ;; start with number. just up to 5. I can'n read quick more then 5, I think.
      ("\\`1\\."         . "1")
      ("\\`2\\."         . "2")
      ("\\`3\\."         . "3")
      ("\\`4\\."         . "4")
      ("\\`5\\."         . "5"))
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
           (char (or (cdr (seq-find (lambda (rule)
                                      (string-match-p (car rule) name))
                                    my-agent-shell--permission-keys))
                     (map-elt action :char)))
           (label (my-agent-shell--short-permission-label name)))
      (when action
	(when char
          (map-put! action :char char))
        (map-put! action :label (if char (format "%s (%s)" label char) label)))
      action))

  (advice-add 'agent-shell--make-permission-action
              :around #'my-agent-shell--permission-advice)

  ;;; agent-shell: 选项过多/过长时每项一行
  (defcustom my-agent-shell--permission-multiline-min-options 5
    "Split permission options onto separate lines when there are this many.

The diff `View' button counts as an option.  See
`my-agent-shell--permission-dialog-advice'."
    :type 'integer)

  (defcustom my-agent-shell--permission-multiline-max-width 100
    "Split permission options onto separate lines when the row is wider.

Width is measured in columns (`string-width'), so CJK text counts
double.  See `my-agent-shell--permission-dialog-advice'."
    :type 'integer)

  (defcustom my-agent-shell--permission-option-indent "    "
    "Indent for permission options rendered one per line."
    :type 'string)

  (defun my-agent-shell--permission-button-starts (text)
    "Return positions where TEXT's permission buttons start.

Buttons from `agent-shell--make-button' carry a `button' property of
`permission', so find where that property starts rather than parsing
the rendered text."
    (let (starts)
      (dotimes (i (length text))
        (when (and (eq (get-text-property i 'button text) 'permission)
                   (or (zerop i)
                       (not (eq (get-text-property (1- i) 'button text) 'permission))))
          (push i starts)))
      (nreverse starts)))

  (defun my-agent-shell--permission-option-row (text pos)
    "Return the line of TEXT holding the button at POS."
    (let ((beg (if (string-match "\\(?:\n[^\n]*\\)\\'" (substring text 0 pos))
                   (match-beginning 0)
                 0)))
      (substring text beg (or (string-match "\n" text pos) (length text)))))

  (defun my-agent-shell--permission-split-options (text starts)
    "Return TEXT with each permission button of STARTS on its own line.

Only the separator before each button is replaced, so button text, text
properties and the offsets `agent-shell--make-permission-button' derives
from the button length are untouched."
    (let ((indent (or my-agent-shell--permission-option-indent ""))
          (parts nil)
          (beg 0))               ; start of the piece not yet copied
      (dolist (start (cdr starts))
        (let ((sep (if (eq (aref text (1- start)) ?\s) (1- start) start)))
          (push (substring text beg sep) parts)
          (push (concat "\n" indent) parts)
          (setq beg start)))
      (push (substring text beg) parts)
      (apply #'concat (nreverse parts))))

  (defun my-agent-shell--permission-dialog-advice (orig &rest args)
    "Split the permission option row into one line per option when long.

Advises `agent-shell--make-tool-call-permission-text', the only place
that sees every option: it joins all buttons into a single row.  Split
when more than `my-agent-shell--permission-multiline-min-options' buttons
are shown (the diff `View' button counts too) or when the row is wider
than `my-agent-shell--permission-multiline-max-width' columns."
    (let* ((text (apply orig args))
           (starts (my-agent-shell--permission-button-starts text)))
      (if (and (cdr starts)
               (or (>= (length starts) my-agent-shell--permission-multiline-min-options)
                   (> (string-width
                       (my-agent-shell--permission-option-row text (car starts)))
                      my-agent-shell--permission-multiline-max-width)))
          (my-agent-shell--permission-split-options text starts)
        text)))

  (advice-add 'agent-shell--make-tool-call-permission-text
              :around #'my-agent-shell--permission-dialog-advice)
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
