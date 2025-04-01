(setq native-comp-jit-compilation nil)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 50 1024 1024))

(provide 'early-init-file)
