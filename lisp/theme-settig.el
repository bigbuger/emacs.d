;;; them-settig.el --- theme setting

;;; Commentary:
;; 

;;; Code:

(set-face-attribute 'default nil :height 145)

(add-to-list 'load-path "~/.emacs.d/lisp/tomorrow-theme")
;;(require 'tomorrow-night-eighties-theme)
(load-theme 'spacemacs-light t)

;; smart-modeline
;;(setq sml/theme 'respectful)
;;(setq sml/no-confirm-load-theme t)
;;(sml/setup)


(require 'doom-modeline)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 15)

(defun my-doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (+ (frame-char-height) 2))
(advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon nil)

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

(add-hook 'after-init-hook #'doom-modeline-mode)

(provide 'theme-settig)

;;; theme-settig.el ends here
