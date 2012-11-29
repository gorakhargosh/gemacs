(progn
  (when window-system
    (when (goog/platform/is-darwin-p)
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
      (setq exec-path (append exec-path '("/usr/local/bin")))
      ))
  )
(provide 'mia)
