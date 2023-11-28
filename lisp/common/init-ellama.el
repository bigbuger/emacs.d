;;; init-ellama.el --- ellama, ollama 大模型

;;; Commentary:
;; 

;;; Code:

(use-package ellama
  :init
  (setopt ellama-language "Chinese")
  (require 'llm-ollama))

(provide 'init-ellama)

;;; init-ellama.el ends here
