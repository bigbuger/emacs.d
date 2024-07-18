;;; init-package.el --- package to install

;;; Commentary:
;;

;;; Code:

(defvar my-packages
  '(
    ;; base
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
    olivetti
    ov

    ;; flyspell
    flyspell-correct-popup

    ;; lsp
    lsp-mode
    dap-mode
    lsp-ui

    ;; about vc
    magit
    diff-hl
    ztree
        
    ;; about project
    projectile
    projectile-ripgrep
    ibuffer-projectile
    
    ;; ruby
    inf-ruby
    rvm
    yard-mode
    
    ;; go
    go-mode
    gotest
    go-scratch
    go-dlv
    go-eldoc
    go-gen-test
    go-impl
    go-tag
    go-fill-struct
    gorepl-mode

    ;;rust
    rust-mode
    racer
    rustic
    flycheck-rust
    
    ;; haskell
    haskell-mode
    company-ghci
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
    ;;clj-refactor
    
    
    ;; python
    pyvenv
    lsp-pyright

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
    
    ;; others
    csv-mode
    json-mode
    yaml-mode
    markdown-mode
    ;; company-ansible
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
