;;; init-lsp.el --- lsp 配置，使用 lsp-mode

;;; Commentary:
;; 
;;; Code:

(require 'lsp-mode)
(require 'lsp-ui)
(require 'dap-mode)

(setq lsp-warn-no-matched-clients nil)
(setq lsp-auto-guess-root nil)
(setq lsp-guess-root-without-session nil)
(setq lsp-prefer-flymake :none)
(setq lsp-ui-flycheck-enable t)
(setq lsp-diagnostic-clean-after-change t)
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

(setopt lsp-signature-posframe-params '(:poshandler posframe-poshandler-point-bottom-left-corner-upward :height 2 :width 120 :border-width 1 :min-width 60))
;; (setq lsp-signature-function 'lsp-signature-posframe)
(defun my-lsp-signature-activate ()
  (interactive)
  (setq lsp-signature-function 'lsp-signature-posframe)
  (lsp-signature-activate))
(add-hook 'lsp-signature-mode-hook
	  #'(lambda ()
	      (unless lsp-signature-mode
		(setq lsp-signature-function 'lsp-lv-message))))

(define-key lsp-mode-map (kbd "s-d") 'my-lsp-signature-activate)
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

(use-package consult-lsp
  :after (lsp consult)
  :init
  (define-key lsp-command-map (kbd "s") 'consult-lsp-symbols)
  (define-key lsp-command-map (kbd "e") 'consult-lsp-diagnostics))

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
    ("a" "Add"    dap-breakpoint-add)]
   [("c" "Set condition"   dap-breakpoint-condition)
    ("h" "Set hit count"   dap-breakpoint-hit-condition)
    ("l" "Set log message" dap-breakpoint-log-message)]])

(transient-define-prefix dap-transient-switch ()
  )

(transient-define-prefix dap-transient ()
  "Transient for dap."
  ["Debug" :class transient-row
   ( "n" "↴ Next"       dap-next                  :transient t)
   ( "i" "⇣ Step in"    dap-step-in               :transient t)
   ( "o" "⇡ Step out"   dap-step-out              :transient t)
   ( "c" "➤ Continue"   dap-continue              :transient t)
   ( "r" "↺ restart"    dap-debug-restart         :transient t)
   ( "b" "Breakpoints›" dap-transient-breakpoints :transient t)]

  [["Select"
    ("ss" "Select Session"     dap-switch-session     :transient t)
    ("st" "Select Thread"      dap-switch-thread      :transient t)
    ("sf" "Select Stack frame" dap-switch-stack-frame :transient t)
    ("su" "Up stack frame"     dap-up-stack-frame     :transient t)
    ("sd" "Down stack frame"   dap-down-stack-frame   :transient t)]
   
   ["Switch"
    ("sl" "List locals"      dap-ui-locals      :transient t)
    ("sb" "List breakpoints" dap-ui-breakpoints :transient t)
    ("sS" "List sessions"    dap-ui-sessions    :transient t)
    ("se" "List expressions" dap-ui-expressions :transient t)]
   
   ["Eval"
    ("ee" "Eval"           dap-eval               :transient t)
    ("ea" "Add expression" dap-ui-expressions-add :transient t)
    ("er" "Repl"           dap-ui-repl)]]

  [""
   [("C-n" "next line"     next-line          :transient t)
    ("C-p" "previous line" previous-line      :transient t)]
   [("C-f" "forward char"  forward-char       :transient t)
    ("C-b" "forwardchar"   backward-char      :transient t)]
   [("q"   "Quit"          transient-quit-all :transient nil)
    ("Q"   "Kill"          dap-disconnect     :transient nil)]])

(transient-define-prefix dap-transient-simple ()
  "Transient for dap."
  ;; :display-action '(display-buffer-in-side-window
  ;; 		    (side . top)
  ;; 		    (dedicated . t)
  ;; 		    (inhibit-same-window . t))
  :transient-non-suffix 'transient--do-stay
  ["Debug" :class transient-row
   ("↴"        "Next"       dap-next          :transient t)
   ("⇣"        "Step in"    dap-step-in       :transient t)
   ("⇡"        "Step out"   dap-step-out      :transient t)
   ("➤"        "Continue"   dap-continue      :transient t)
   ("↺"        "restart"    dap-debug-restart :transient t)
   ("☩"        "kill"       dap-disconnect    :transient t)
   ("λ"        "repl"       dap-ui-repl       :transient nil)
   ("C-M-<f5>" "Full menu"  dap-transient     :transient nil)])

(define-key lsp-mode-map (kbd "C-M-<f5>") 'dap-transient)
(define-key lsp-mode-map (kbd "C-<f5>") 'dap-transient-simple)
;; (add-hook 'dap-stopped-hook
;;           (lambda (arg) (call-interactively #'dap-transient-simple)))

(dolist (cmd '(dap-switch-stack-frame dap-switch-session dap-switch-thread))
  (setf (alist-get cmd vertico-multiform-commands)
        '((vertico-sort-override-function . identity))))

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
    :parent embark-general-map
    "<RET>" #'lsp-find-definition
    "n"     #'lsp-ui-find-next-reference
    "p"     #'lsp-ui-find-prev-reference
    "t"     #'lsp-find-type-definition
    "I"     #'lsp-find-implementation
    "r"     #'xref-find-references
    "v"     #'dap-ui-eval-variable-in-buffer
    "e"     #'dap-eval-thing-at-point
    "E"     #'dap-eval-region
    "h"     #'lsp-treemacs-call-hierarchy
    "H"     #'embark-toggle-highlight
    "c"     #'string-inflection-lower-camelcase
    "C"     #'string-inflection-camelcase
    "u"     #'string-inflection-underscore
    "U"     #'string-inflection-upcase
    "k"     #'string-inflection-kebab-case
    "R"     #'lsp-rename
    "o"     #'occur
    )

  (add-to-list 'embark-target-injection-hooks '(lsp-rename embark--allow-edit))
  (add-to-list 'embark-repeat-actions #'lsp-ui-find-prev-reference)
  (add-to-list 'embark-repeat-actions #'lsp-ui-find-next-reference)

  (unless (memq 'embark-org-target-element-context embark-target-finders)
    (if-let ((tail (memq 'embark-target-active-region embark-target-finders)))
	(push 'embark-target-lsp-identifier-at-point (cdr tail))
      (push 'embark-target-lsp-identifier-at-point embark-target-finders)))
  
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

(define-key lsp-mode-map (kbd "M-<RET>") #'lsp-execute-code-action)

(provide 'init-3-lsp)

;;; init-3-lsp.el ends here
