(defvar my-packages
  '(
    ;; base
    color-theme
    undo-tree
    ivy
    swiper
    counsel
    smex
    company
    company-quickhelp
    yasnippet
    visual-regexp
    flycheck
    flymake-easy
    flymake-cursor
    popup
    exec-path-from-shell
    magit
    projectile
    neotree
    all-the-icons
    realgud
    epl
    osx-dictionary
    ;; ruby
    inf-ruby
    robe
    rvm
    ruby-electric
    rsense
    flymake-ruby
    realgud-byebug
    ;; go
    go-mode
    gotest
    go-scratch
    go-playground
    go-dlv
    go-eldoc
    company-go
    ;; haskell
    haskell-mode
    company-ghc
    company-ghci
    ghc
    flymake-haskell-multi
    ;; scheme
    geiser
    sicp
    )
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
