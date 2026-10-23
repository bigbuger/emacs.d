package `
(let ((pkg (ignore-errors
	     (with-temp-buffer
               (call-process "go" nil '(t nil) nil "list" "-f" "{{.Name}}")
               (buffer-string)))))
  (if (string-empty-p pkg)
      (car (last (split-string (file-name-directory buffer-file-name) "/") 2))
    (car (split-string pkg "\n"))))
`   
$0
