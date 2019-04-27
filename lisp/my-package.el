(defvar my-packages
  '(
    ;; base
    color-theme
    undo-tree
    ivy
    swiper
    counsel
    ivy-rich
    which-key
    ag
    smex
    company
    company-quickhelp
    yasnippet
    auto-yasnippet
    visual-regexp
    multi-term
    multiple-cursors
    flycheck
    flymake-easy
    realgud
    popup
    exec-path-from-shell
    neotree
    all-the-icons
    string-inflection
    move-text
    
    ;; about project
    magit
    projectile
    counsel-projectile
    
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
    
    ;; python
    elpy
    company-jedi

    ;; scala
    ensime
    
    ;; other
    json-mode
    epl
    osx-dictionary
    yaml-mode
    markdown-mode
    company-web
    restclient
    company-restclient
    docker
    )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  "Install need packages."
  (cl-loop for p in my-packages
        when (not (package-installed-p p)) do (cl-return nil)
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
