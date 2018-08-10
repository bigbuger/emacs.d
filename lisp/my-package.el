(defvar my-packages
  '(company-go
    company
    all-the-icons
    neotree
    counsel
    ivy
    magit
    projectile
    rvm
    dash
    epl
    flymake-easy
    go-mode
    gotest
    go-scratch
    go-playground
    go-eldoc
    go-dlv
    haskell-mode
    json-reformat
    json-snatcher
    pkg-info
    popup
    prop-menu
    flycheck-rebar3
    flymake-cursor
    json-mode
    idris-mode
    flymake-haskell-multi
    flycheck
    yasnippet
    visual-regexp
    undo-tree
    smex
    robe
    exec-path-from-shell
    inf-ruby
    ruby-electric
    rsense
    ghc
    flymake-ruby
    color-theme)
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
