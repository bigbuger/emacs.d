;;; them-settig.el --- theme setting  -*- lexical-binding: t; -*-

;;; Commentary:
;; 

;;; Code:

(if (member "Menlo" (font-family-list))
    (progn
      (set-face-attribute 'default nil :height 150 :font "Menlo")
      (set-fontset-font t 'han "PingFang SC")
      )
  (set-face-attribute 'default nil :height 150))
(setq-default line-spacing 1) ;; 行间距
(setq custom--inhibit-theme-enable nil)

(require 'spacemacs-light-theme)
(custom-set-variables
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t))

(load-theme 'spacemacs-light t)
(custom-theme-set-faces
 'spacemacs-light

 ;; 当前行
 '(line-number-current-line ((t (:inherit line-number :background "#f1d9e4" :foreground "#655370"))))
 
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

 ;; jinx
 '(jinx-misspelled ((t (:underline (:color "#65aa6c" :style wave :position nil)))))
 
 ;; 代码高亮色修改
 '(font-lock-constant-face ((t (:foreground "#6c71c4"))))
 '(font-lock-function-name-face ((t (:inherit nil :foreground "#9a4622"))))
 '(font-lock-keyword-face ((t (:inherit nil :foreground "#3a81c3" :slant normal))))
 '(font-lock-type-face ((t (:inherit nil :foreground "#859900"))))

 ;; lsp 当前变量高亮
 '(lsp-face-highlight-textual ((t (:inherit nil :background "#e6ebeb"))))
 '(lsp-face-highlight-read ((t (:inherit nil :background "#E5EBEB"))))
 '(lsp-face-highlight-write ((t (:inherit nil :background "#f9d9e4"))))
 ;; lsp inlay hint
 '(lsp-inlay-hint-face ((t (:background "#E3E0CE"  :foreground "#839496"))))

 '(show-paren-match ((t (:inherit bold :foreground "royal blue" :underline t))))
 '(term-color-blue ((t (:background "SkyBlue3" :foreground "SkyBlue3"))))
 '(treemacs-directory-face ((t (:inherit default))))

 ;; org 设置代码块用上下边线包裹
 '(org-block-begin-line ((t (:underline (:color "#ded2b0")
					:background "#fef9eb"))))
 '(org-block ((t :background "#fef9eb")))
 '(org-block-end-line ((t (:overline nil :underline nil :background nil))))

 ;; magit 当前选择高亮
 '(magit-section-highlight ((t :background "#d3d3e7")))

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
(doom-modeline-mode)

(provide 'theme-setting)

;;; theme-setting.el ends here
