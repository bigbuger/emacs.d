(defvar my-packages
  '(flycheck-rebar3 auto-complete-distel erlang flymake-cursor robe json-mode idris-mode flymake-haskell-multi flycheck yasnippet visual-regexp undo-tree sr-speedbar smex ruby-electric rsense go-scratch go-playground go-eldoc go-dlv go-autocomplete ghc flymake-ruby color-theme ac-inf-ruby ac-haskell-process)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs  is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))



(provide 'my-packages)
