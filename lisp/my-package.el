;;; my-package.el --- package to install

;;; Commentary:
;;

;;; Code:

(defvar my-packages
  '(
    ;; base
    undo-tree
    popup
    crux
    string-inflection
    move-text
    multiple-cursors
    expand-region
    smartparens
    auto-highlight-symbol
    ripgrep
    visual-regexp
    regex-tool
    pyim
    eldoc-box
    
    which-key
    hydra
    
    imenu-list
    centaur-tabs

    dumb-jump

    smart-compile
    rmsbolt

    helpful
    
    ;; file manager
    treemacs
    dirvish
    
    ;; indent
    highlight-indent-guides

    ;; theme
    spacemacs-theme
    doom-modeline
    all-the-icons
    all-the-icons-dired
    all-the-icons-ibuffer
    
    ;; ivy
    ivy
    swiper
    counsel
    ivy-hydra
    counsel-fd
    wgrep
    ivy-rich
    all-the-icons-ivy-rich
    counsel-dash

    ;; company
    company
    company-quickhelp
    company-fuzzy

    ;; yasnippet
    yasnippet
    auto-yasnippet

    ;; shell term
    vterm

    ;; flycheck
    flycheck
    
    ;; realgud
    realgud

    ;; org
    org-bullets
    gnuplot-mode
    olivetti

    ;; flyspell
    flyspell-correct-popup
    

    ;; resetclient
    restclient
    company-restclient
    ob-restclient
    verb
    impostman
    
    ;; lsp
    lsp-mode
    dap-mode
    lsp-ui

    ;; tree-sitter
    tree-sitter
    tree-sitter-langs

    ;; about vc
    magit
    magit-todos
    magit-delta
    diff-hl
    ztree
    
    
    ;; about project
    projectile
    projectile-ripgrep
    counsel-projectile
    ibuffer-projectile
    
    ;; ruby
    inf-ruby
    robe
    rvm
    rsense
    realgud-byebug
    yard-mode
    company-inf-ruby
    
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
    go-gen-test
    go-impl
    godoctor
    go-tag
    go-fill-struct
    ob-go
    gorepl-mode

    ;;rust
    rust-mode
    racer
    rustic
    flycheck-rust
    
    ;; haskell
    haskell-mode
    company-ghc
    company-ghci
    ghc
    dante
    ;; flymake-haskell-multi
    
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

    ;; elm lang
    elm-mode

    ;; web
    web-mode
    company-web

    ;; sql
    ejc-sql
    sqlformat
    
    ;; laTeX
    company-math
    auctex
    magic-latex-buffer
    xenops
    
    ;; docker
    docker
    docker-compose-mode
    dockerfile-mode
    
    ;; others
    csv-mode
    json-mode
    graphviz-dot-mode
    yaml-mode
    markdown-mode
    company-ansible
    protobuf-mode

    rainbow-mode
    osx-dictionary
    google-translate
    osx-trash
    exec-path-from-shell
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
