;;; them-settig.el --- theme setting

;;; Commentary:
;; 

;;; Code:

(set-face-attribute 'default nil :height 145)

;; (add-to-list 'load-path "~/.emacs.d/lisp/tomorrow-theme")
;; (require 'tomorrow-night-eighties-theme)
;; (load-theme 'spacemacs-light t)
(require 'doom-themes)
(setq doom-themes-enable-bold nil)
(load-theme 'doom-solarized-light t)

(require 'doom-modeline)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 18)

(defun my-doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (+ (frame-char-height) 2))
(advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format nil)

;; (setq doom-modeline-icon nil)

;; change of branch reflect in modeline
(setq auto-revert-check-vc-info t)
(setq doom-modeline-vcs-max-length 50)
(add-hook 'after-init-hook #'doom-modeline-mode)

(provide 'theme-settig)

;;; theme-settig.el ends here
