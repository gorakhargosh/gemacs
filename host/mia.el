(progn
  (when window-system
    (when (goog/platform/is-darwin-p)
      ;; Monaco is clean. The default is too small in the GUI.
      (set-face-font 'default "Consolas-16")
      ;; Mac OS X-specific font anti-aliasing.
      (setq mac-allow-anti-aliasing t)))
  )
(provide 'mia)
