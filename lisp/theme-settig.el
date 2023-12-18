;;; them-settig.el --- theme setting

;;; Commentary:
;; 

;;; Code:

(if (member "Menlo" (font-family-list))
    (set-face-attribute 'default nil :height 150 :font "Menlo")
  (set-face-attribute 'default nil :height 150))

(setq custom--inhibit-theme-enable nil)

(require 'spacemacs-light-theme)
(load-theme 'spacemacs-light t)
(custom-theme-set-faces
 'spacemacs-light
 
 ;; auto highlight 高亮色
 '(ahs-face ((t (:background "#e6ebeb"))))
 '(ahs-plugin-default-face ((t (:background "#efebfb"))))
 
 '(aw-leading-char-face ((t (:foreground "red" :height 5.0))))
 '(diff-hl-change ((t (:background "blue3" :foreground "blue3"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "red3" :foreground "red3"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "green4" :foreground "green4"))))

 ;; flycheck
 '(flycheck-pos-tip-max-width 20)
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline (:color "#3a81c3" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "yellow4" :style wave)))))
 '(flycheck-posframe-error-face ((t (:inherit flycheck-posframe-face :foreground "VioletRed1"))))
 '(flycheck-posframe-face ((t (:inherit default :background "#efeae9"))))

 ;; 代码高亮色修改
 '(font-lock-constant-face ((t (:foreground "#6c71c4"))))
 '(font-lock-function-name-face ((t (:inherit nil :foreground "#9a4622"))))
 '(font-lock-keyword-face ((t (:inherit nil :foreground "#3a81c3" :slant normal))))
 '(font-lock-type-face ((t (:inherit nil :foreground "#859900"))))

 ;; lsp 当前变量高亮 
 '(lsp-face-highlight-textual ((t (:inherit highlight :background "#e6ebeb"))))

 '(show-paren-match ((t (:inherit bold :foreground "royal blue" :underline t))))
 '(term-color-blue ((t (:background "SkyBlue3" :foreground "SkyBlue3"))))
 '(treemacs-directory-face ((t (:inherit default))))

 ;; org 设置代码块用上下边线包裹
 '(org-block-begin-line ((t (:underline "#cccccc" :background "#e1e0e1"))))
 '(org-block-end-line ((t (:overline nil :underline nil :background unspecified))))

 ;; xeft 高亮色
 '(xeft-inline-highlight ((t (:inherit underline :extend t :foreground "#3a81c3"))))
 '(awesome-tab-selected-face ((t (:inherit default
				  :height 160
					  :underline "#3a81c3"
					  :foreground "#3a81c3"
					  :background "#fbf8ef"
					  :distant-foreground "#3a81c3"
					  :weight bold))))
 '(awesome-tab-unselected-face ((t (:inherit default
				    :height 160
					    :foreground "#a094a2"
					    :background "#efeae9"))))

 '(consult-file ((t (:inherit font-lock-keyword-face)))))

 
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
