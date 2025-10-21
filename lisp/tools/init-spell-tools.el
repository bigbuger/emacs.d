;;; init-spell-tools.el --- spell and dictionary tools

;; osx-dictionary

;;; Commentary:
;; 

(require 'pos-tip)
;;; Code:

(setq pos-tip-tab-width 800)
(require 'osx-dictionary)
(defun osx-dictionary-search-at-point-and-pop ()
  "Search word around and display result with popup."
  (interactive)
  (let* ((word (osx-dictionary--region-or-word))
	 (result (osx-dictionary--search word)))
    (pos-tip-show result)))
(global-set-key (kbd "C-S-d") 'osx-dictionary-search-at-point-and-pop)

;; flyspell
(setq flyspell-mark-duplications-flag nil)
;; (require 'flyspell-correct-popup)
;; (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

(setq ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=ultra" "--camel-case"))

;; Jinx is a fast just-in-time spell-checker for Emacs.
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (unbind-key "<down-mouse-3>" jinx-overlay-map) ;; disable mouse
  (put 'jinx-overlay 'mouse-face nil)            ;; disable mouse hover face
  (add-to-list 'jinx-camel-modes 'protobuf-mode)
  
  (setq jinx-exclude-regexps
	'((emacs-lisp-mode "Package-Requires:.*$")
	  (t "[A-Z]+\\>"         ;; Uppercase words
	     "-+\\>"             ;; Hyphens used as lines or bullet points
	     "\\w*?[0-9]\\w*\\>" ;; Words with numbers, hex codes
	     "[a-z]+://\\S-+"    ;; URI
	     "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?" ;; Email
	     "\\(?:Local Variables\\|End\\):\\s-*$" ;; Local variable indicator
	     "jinx-\\(?:languages\\|local-words\\):\\s-+.*$" ;; Local variables
	     "\\cc" ;; Chinese
	     ".*/.*\\>" ;; File path
	     "\\<[a-zA-Z]\\{1,3\\}\\>" ;; short word
	     ))
	)
  
  (setq jinx-include-faces
	'((prog-mode font-lock-comment-face font-lock-doc-face font-lock-string-face font-lock-constant-face
		     font-lock-variable-name-face font-lock-function-name-face)
	  (conf-mode font-lock-comment-face font-lock-string-face)

	  ;; `yaml-mode' and `yaml-ts-mode' are text-modes,
	  ;; while they should better be conf- or prog-modes.
	  (yaml-mode . conf-mode)
	  (yaml-ts-mode . conf-mode)))

  (defvar jinx-min-word-length 3
    "Jinx check mint length.")
  
  (defun my-jinx--ignore-case-word-valid-p (start)
    "Return non-nil if word, that is assumed to be in lower case, at
START is valid, or would be valid if capitalized or upcased."
    (let ((word (buffer-substring-no-properties start (point))))
      (or (member word jinx--session-words)
	  (<= (length word) jinx-min-word-length)
	  (cl-loop for dict in jinx--dicts thereis
		   (or
		    (jinx--mod-check dict (upcase word))
		    (jinx--mod-check dict (downcase word))
		    (jinx--mod-check dict (capitalize word))
		    (jinx--mod-check dict word))))))

  (setq jinx--predicates
	(list #'jinx--face-excluded-p
              #'jinx--regexp-excluded-p
	      #'jinx--property-excluded-p
	      #'my-jinx--ignore-case-word-valid-p
              #'jinx--word-valid-p))
  
  )

(use-package vertico
  :config
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20))))

;; end flyspell

(provide 'init-spell-tools)

;;; init-spell-tools.el ends here
