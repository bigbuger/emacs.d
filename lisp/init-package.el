;;; init-package.el --- package to install

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

    dumb-jump

    smart-compile
    rmsbolt

    helpful
    
    ;; file manager
    treemacs
    treemacs-all-the-icons
    dirvish
    
    ;; indent
    highlight-indent-guides

    ;; theme
    spacemacs-theme
    color-theme-sanityinc-tomorrow
    doom-modeline
    all-the-icons
    all-the-icons-dired
    all-the-icons-ibuffer

    ;; wgrep
    wgrep

    ;; company
    company
    company-quickhelp
    company-fuzzy

    ;; yasnippet
    yasnippet
    auto-yasnippet
    yatemplate

    ;; shell term
    vterm

    ;; flycheck
    flycheck
    
    ;; realgud
    realgud

    ;; org
    gnuplot-mode
    olivetti
    ov

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

    ;; about vc
    magit
    magit-todos
    magit-delta
    diff-hl
    ztree
        
    ;; about project
    projectile
    projectile-ripgrep
    ibuffer-projectile
    
    ;; ruby
    inf-ruby
    robe
    rvm
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
    company-ghci
    ;; ghc
    dante
    lsp-haskell
    company-cabal
    idris-mode
    
    ;; scheme
    geiser
    geiser-guile
    flycheck-guile
    sicp

    ;; clojure
    clojure-mode
    cider
    clj-refactor
    
    
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
    sqlformat
    
    ;; laTeX
    company-math
    auctex
    magic-latex-buffer
    xenops
    
    ;; others
    csv-mode
    json-mode
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


(provide 'init-package)

;;; init-package.el ends here
