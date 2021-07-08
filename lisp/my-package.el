;;; my-package.el --- package to install

;;; Commentary:
;;

;;; Code:

(defvar my-packages
  '(
    ;; base
    color-theme
    undo-tree
    popup
    all-the-icons
    exec-path-from-shell
    crux
    dired-subtree
    
    smex
    which-key
    ivy
    swiper
    counsel
    wgrep
    ivy-rich
    hydra

    company
    company-quickhelp
    yasnippet
    auto-yasnippet
    visual-regexp

    multi-term
    multiple-cursors

    flycheck
    flycheck-pos-tip
    flymake-easy
    realgud

    neotree
    string-inflection
    move-text
    expand-region
    centaur-tabs
    aggressive-indent
    highlight-indent-guides
    all-the-icons-dired

    ;;org
    org-bullets
    gnuplot
    gnuplot-mode
    olivetti
    
    ;; lsp
    lsp-mode
    dap-mode
    company-lsp
    lsp-ui
    
    ;; about project
    magit
    projectile
    counsel-projectile
    diff-hl
    
    ;; ruby
    inf-ruby
    robe
    rvm
    ;;ruby-electric
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
    go-guru
    go-rename

    ;;rust
    rust-mode
    racer
    
    ;; haskell
    haskell-mode
    company-ghc
    company-ghci
    ghc
    ;;flymake-haskell-multi
    
    ;; scheme
    geiser
    geiser-guile
    sicp
    
    ;; python
    elpy
    company-jedi

    ;; c lang
    ccls
    
    ;; other
    json-mode
    counsel-jq
    
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


(provide 'my-package)

;;; my-package.el ends here
