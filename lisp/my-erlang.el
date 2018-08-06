(add-to-list 'load-path "~/.emacs.d/lisp/distel/elisp/")
(require 'distel)
(distel-setup)


(require 'auto-complete-distel)
(add-to-list 'ac-sources 'auto-complete-distel)

(setq distel-completion-get-doc-from-internet t)

(add-hook 'erlang-mode-hook 'flycheck-mode)
(add-to-list 'ac-modes 'erlang-mode)
