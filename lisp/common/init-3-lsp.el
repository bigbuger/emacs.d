;;; init-lsp.el --- lsp 配置，使用 lsp-mode

;;; Commentary:
;; 
;;; Code:

(require 'lsp-mode)
(require 'lsp-ui)
(require 'dap-mode)

(setq lsp-warn-no-matched-clients nil)
(setq lsp-auto-guess-root t)
(setq lsp-guess-root-without-session t)
(setq lsp-prefer-flymake :none)
(setq lsp-ui-flycheck-enable t)
;; (setq lsp-inlay-hint-enable t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-headerline-breadcrumb-enable t)
;; (add-hook 'lsp-mode-hook
;; 	  '(lambda ()
;; 	     (setq-local header-line-format
;; 			 '((t
;; 			    (:eval
;; 			     (frame-parameter nil 'lsp-headerline--string)))))))

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-show-with-cursor nil)

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-code-actions t)

(setq lsp-ui-doc-include-signature t)

(setq lsp-signature-auto-activate t)
(setq lsp-signature-doc-lines 2)
(setq lsp-signature-render-documentation t)
;; (setq lsp-signature-function 'lsp-signature-posframe)
(define-key lsp-mode-map (kbd "s-d") 'lsp-signature-activate)
(define-key lsp-mode-map (kbd "s-l l") 'lsp-ui-sideline-mode)

(setq lsp-auto-execute-action nil)
(setq lsp-modeline-code-actions-segments '(count icon))

;; (setq lsp-completion-sort-initial-results nil)
(setq lsp-completion-provider :none)
(setq lsp-completion-default-behaviour :insert)
;; (add-hook 'lsp-completion-mode-hook
;; 	  #'(lambda ()
;; 	     (setq-local company-backends
;; 			 (cl-adjoin '(company-capf :separate company-yasnippet)
;; 				    company-backends :test #'equal))))

(setq dap-auto-configure-features '()
      dap-ui-variable-length 10000)
;; (define-key lsp-mode-map (kbd "M-?") 'lsp-ui-peek-find-references)
(define-key lsp-mode-map [f5] 'dap-debug)

;; (define-key lsp-mode-map (kbd "C-<f5>") 'dap-hydra)
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-hydra)))

;; use transient, hydra not band other key
(transient-define-prefix dap-transient-breakpoints ()
  ["Breakpoints"
   [("t" "Toggle" dap-breakpoint-toggle)
    ("d" "Delete" dap-breakpoint-delete)
    ("a" "Add" dap-breakpoint-add)]
   [("c" "Set condition" dap-breakpoint-condition)
    ("h" "Set hit count" dap-breakpoint-hit-condition)
    ("l" "Set log message" dap-breakpoint-log-message)]])

(transient-define-prefix dap-transient-switch ()
  ["Switch"
   [("s" "Session" dap-switch-session :transient t)
    ("t" "Thread" dap-switch-thread :transient t)]
   [("f" "Stack frame" dap-switch-stack-frame :transient t)
    ("u" "Up stack frame" dap-up-stack-frame :transient t)
    ("d" "Down stack fram" dap-down-stack-frame :transient t)]
   [("l" "List locals" dap-ui-locals :transient t)
    ("b" "List breakpoints" dap-ui-breakpoints :transient t)
    ("S" "List sessions" dap-ui-sessions :transient t)
    ("e" "List expressions" dap-ui-expressions :transient t)]])

(transient-define-prefix dap-transient ()
  "Transient for dap."
  ["Stepping" :class transient-row
   ("n" "↴ Next"    dap-next :transient t)
   ("i" "⇣ Step in"  dap-step-in :transient t)
   ("o" "⇡ Step out" dap-step-out :transient t)
   ("c" "➤ Continue" dap-continue :transient t)
   ("r" "↺ restart"  dap-debug-restart :transient t)]

  ["Debug" :class transient-row
   ("b" "Breakpoints›" dap-transient-breakpoints :transient t)
   ("s" "Switch›" dap-transient-switch :transient t)]

  ["Eval" :class transient-row
    ("ee" "Eval" dap-eval :transient t)
    ("ea" "Add expression" dap-ui-expressions-add :transient t)
    ("er" "Repl" dap-ui-repl)]

  [:class transient-row
   ("C-n" "next line" next-line :transient t)
   ("C-p" "previous line" previous-line :transient t)
   ("q" "Quit" transient-quit-all :transient nil)
   ("Q" "Kill" dap-disconnect :transient nil)])

(define-key lsp-mode-map (kbd "C-<f5>") 'dap-transient)
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-transient)))

(defun +disable-vertico-sort (fun &rest args)
  (minibuffer-with-setup-hook
      (:append (lambda ()
		 (setq-local vertico-sort-function nil)))
    (apply fun args)))
(advice-add 'dap-switch-stack-frame :around #'+disable-vertico-sort)
(advice-add 'dap-switch-session :around #'+disable-vertico-sort)
(advice-add 'dap-switch-thread :around #'+disable-vertico-sort)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients 'semgrep-ls))


;; Add identifiers in LSP-mode as their own emabrk's target type
(with-eval-after-load 'embark
  (defun embark-target-lsp-identifier-at-point ()
    (when lsp-mode
      (when-let ((syms (embark-target-identifier-at-point)))
        (cons 'lsp-identifier (cdar syms)))))

  (defvar-keymap embark-lsp-indetifier-actions
    :doc "Keymap for actions for lsp-identifier."
    :parent nil
    "<RET>" #'lsp-find-definition
    "n"     #'lsp-ui-find-next-reference
    "p"     #'lsp-ui-find-prev-reference
    "t"     #'lsp-find-type-definition
    "i"     #'lsp-find-implementation
    "r"     #'xref-find-references
    "v"     #'dap-ui-eval-variable-in-buffer
    "e"     #'dap-eval-thing-at-point
    "E"     #'dap-eval-region
    "h"     #'lsp-treemacs-call-hierarchy)

  (add-to-list 'embark-repeat-actions #'lsp-ui-find-prev-reference)
  (add-to-list 'embark-repeat-actions #'lsp-ui-find-next-reference)
  (add-to-list 'embark-target-finders 'embark-target-lsp-identifier-at-point)
  (add-to-list 'embark-keymap-alist '(lsp-identifier . embark-lsp-indetifier-actions)))

;; topsy 面包屑展示函数名称
(use-package topsy
  :config
  (add-hook 'topsy-mode-hook
	    (lambda ()
	      (setq-local lsp-headerline-breadcrumb-enable nil))))


(use-package vertico-posframe
  :after vertico
  :config
  (defun lsp-code-action-is-quick-fix? (x)
  (string-equal "quickfix" (lsp:code-action-kind? x)))

  (defun vertico-sort-lsp-fix-action-first (list)
    "Sort directories before files in LIST."
    (nconc (cl-loop for x in list if (lsp-code-action-is-quick-fix? x) collect x)
           (cl-loop for x in list if (not (lsp-code-action-is-quick-fix? x)) collect x)))
  
  (setf (alist-get 'lsp-execute-code-action vertico-multiform-commands)
               '(posframe
		 (vertico-posframe-poshandler . posframe-poshandler-point-bottom-left-corner)
		 (vertico-posframe-width . 120)
		 (vertico-sort-override-function . vertico-sort-lsp-fix-action-first))))

(provide 'init-3-lsp)

;;; init-3-lsp.el ends here
