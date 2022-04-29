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
    all-the-icons-dired
    doom-modeline
    ranger
    
    which-key
    ivy
    swiper
    counsel
    counsel-fd
    wgrep
    ivy-rich
    all-the-icons-ivy-rich
    hydra
    ripgrep
    figlet

    company
    company-quickhelp
    yasnippet
    auto-yasnippet
    visual-regexp

    ;; multi-term
    vterm

    flycheck
    flycheck-pos-tip
    flymake-easy
    realgud

    smartparens
    treemacs
    string-inflection
    move-text
    multiple-cursors
    expand-region

    centaur-tabs
    aggressive-indent
    highlight-indent-guides
    imenu-list

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

    ;; tree-sitter
    tree-sitter
    tree-sitter-langs
    
    ;; about project
    magit
    magit-todos
    projectile
    projectile-ripgrep
    counsel-projectile
    diff-hl
    ztree
    
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
    flycheck-rust
    
    ;; haskell
    haskell-mode
    company-ghc
    company-ghci
    ghc
    dante
    ;;flymake-haskell-multi
    
    ;; scheme
    geiser
    geiser-guile
    flycheck-guile
    sicp
    
    ;; python
    elpy
    company-jedi
    lsp-pyright

    ;; c lang
    ccls

    ;; laTeX
    company-math
    auctex
    magic-latex-buffer
    
    ;; other
    csv-mode
    json-mode
    graphviz-dot-mode
    
    epl
    osx-dictionary
    yaml-mode
    markdown-mode

    company-web
    restclient
    company-restclient
    ob-restclient

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
