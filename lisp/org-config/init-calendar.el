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

(defun org-icalendar-open-ics-file (file)
  "Auto open ics FILE and then delete it."
  (start-process "org-icalendar-open-ics-file-process" nil "open" "-F" file))

(add-hook 'org-icalendar-after-save-hook 'org-icalendar-open-ics-file)

(provide 'init-calendar)

;;; init-calendar.el ends here
