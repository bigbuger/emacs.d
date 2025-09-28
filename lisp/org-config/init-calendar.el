;;; init-calendar.el --- 日历配置

;;; Commentary:
;; 

;;; Code:

;; loclization time and calendar
(setq system-time-locale "zh_CN")
(setq calendar-week-start-day 1)
(setq calendar-month-header `(propertize
			      (format "%d 年 %2d 月" year month)
			      'font-lock-face 'calendar-month-header))

;;日程显示日期为中文
;; Month
(setq calendar-month-name-array
      ["1月" "2月" "3月" " 4月" "5月" "6月" "7月" "8月" "9月" "10月" "11月" "12月"])

;; Week days
(setq calendar-day-name-array
      ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])


;; agenda 配置
(require 'org-agenda)
;; (global-set-key (kbd "C-c A") #'org-agenda)

(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)

;; agenda 日期显示格式修改为中文的
(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((day-of-week (calendar-day-of-week date))
	 (dayname (aref calendar-day-name-array
                        (calendar-day-of-week date)))
         (day (cadr date))
         (month (car date))
         (year (nth 2 date)))
    (format "%04d-%02d-%02d %s" year month day dayname)))


(use-package calfw
  :config
  ;; 关闭 holidays 展示
  (setq cfw:display-calendar-holidays nil))

(use-package calfw-org
  :config
  (setq cfw:org-overwrite-default-keybinding t) ;; 使用 org like 按键
  
  ;; 日历边框
  (setq cfw:fchar-junction ?╬
	cfw:fchar-vertical-line ?║
	cfw:fchar-horizontal-line ?═
	cfw:fchar-left-junction ?╠
	cfw:fchar-right-junction ?╣
	cfw:fchar-top-junction ?╦
	cfw:fchar-top-left-corner ?╔
	cfw:fchar-top-right-corner ?╗)

  ;; 修复 cfw 解析时间范围出错
  (defun cfw:org-get-timerange (text)
    "Return a range object (begin end text).
If TEXT does not have a range, return nil."
    (let* ((dotime (cfw:org-tp text 'dotime)))
      (and (stringp dotime) (string-match org-ts-regexp dotime)
	   (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
		  (start-date (nth 1 (car matches)))
		  (end-date (nth 1 (nth 1 matches)))
		  (extra (cfw:org-tp text 'extra)))
	     (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
		 ( list( calendar-gregorian-from-absolute
			 (time-to-days
			  (org-read-date nil t start-date))
			 )
		   (calendar-gregorian-from-absolute
		    (time-to-days
		     (org-read-date nil t end-date))) text)
	       )))))

  ;; 点击改为在新的窗口打开
  (defun cfw:org-onclick ()
    "Jump to the clicked org item."
    (interactive)
    (let (
	  (marker (get-text-property (point) 'org-marker))
	  (link   (get-text-property (point) 'org-link))
	  (file   (get-text-property (point) 'cfw:org-file))
	  (beg    (get-text-property (point) 'cfw:org-h-beg))
	  (loc    (get-text-property (point) 'cfw:org-loc)))
      (when link
	(org-open-link-from-string link))
      (when (and marker (marker-buffer marker))
	(org-mark-ring-push)
	(switch-to-buffer-other-window (marker-buffer marker))
	(widen)
	(goto-char (marker-position marker))
	(when (eq major-mode 'org-mode)
          (org-reveal)))
      (when beg
	(find-file file)
	(goto-char beg)
	(org-cycle))))

  ;; 自定义打开一个 org agenda 大日历
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "CadetBlue2")  ; org-agenda source
      )))
  ;; :bind ("C-c a" . my-open-calendar)
  )

;; 配置等宽字体，保证日历边框不会乱
(with-eval-after-load 'calfw
  (defun width-buffer-face-mode-variable ()
    (interactive)
    (make-face 'width-font-face)
    (set-face-attribute 'width-font-face nil :font "等距更纱黑体 SC 15") ;; 使用更纱黑体对齐表格，mac 安装命令  `brew install --cask font-sarasa-gothic'
    (setq buffer-face-mode-face 'width-font-face)
    (buffer-face-mode))
  (if (member "Sarasa Gothic SC" (font-family-list))
      (add-hook 'cfw:calendar-mode-hook 'width-buffer-face-mode-variable)))


(custom-set-faces
 '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey95" :weight bold)))
 '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey95" :weight bold))))

(defun org-icalendar-open-ics-file (file)
  "Auto open ics FILE and then delete it."
  (start-process "org-icalendar-open-ics-file-process" nil "open" "-F" file))

(add-hook 'org-icalendar-after-save-hook 'org-icalendar-open-ics-file)

(provide 'init-calendar)

;;; init-calendar.el ends here
