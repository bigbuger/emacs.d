;;; them-settig.el --- theme setting

;;; Commentary:
;; 

;;; Code:

(set-face-attribute 'default nil :height 145)

(setq custom--inhibit-theme-enable nil)


;; (add-to-list 'load-path "~/.emacs.d/lisp/tomorrow-theme")
;; (require 'tomorrow-night-eighties-theme)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/emacs-color-theme-solarized")
;; (set-frame-parameter nil 'background-mode 'light)
;; (load-theme 'solarized t)

(require 'spacemacs-light-theme)


(load-theme 'spacemacs-light t)
(custom-theme-set-faces 'spacemacs-light
 '(ahs-face ((t (:background "#e6ebeb"))))
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "green4" :foreground "green4"))))
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline (:color "#3a81c3" :style wave)))))
 '(flycheck-posframe-error-face ((t (:inherit flycheck-posframe-face :foreground "VioletRed1"))))
 '(flycheck-posframe-face ((t (:inherit default :background "#efeae9"))))
 '(flycheck-warning ((t (:underline (:color "yellow4" :style wave)))))
 '(font-lock-constant-face ((t (:foreground "#6c71c4"))))
 '(font-lock-function-name-face ((t (:inherit default :foreground "#9a4622"))))
 '(font-lock-keyword-face ((t (:inherit nil :foreground "#3a81c3" :slant normal))))
 '(font-lock-type-face ((t (:inherit default :foreground "#859900"))))
 '(lsp-face-highlight-textual ((t (:inherit highlight :background "#e6ebeb"))))
 '(show-paren-match ((t (:inherit bold :foreground "royal blue" :underline t))))
 '(term-color-blue ((t (:background "SkyBlue3" :foreground "SkyBlue3"))))
 '(treemacs-directory-face ((t (:inherit default)))))


(require 'doom-themes)
(setq doom-themes-enable-bold nil)
;; (load-theme 'doom-solarized-light t)

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
