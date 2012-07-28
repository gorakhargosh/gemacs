(progn
  (setq user-full-name "Yesudeep Mangalapilly"
        user-mail-address "yesudeep@google.com")
  (when window-system
    (require 'fill-column-indicator)
    (define-globalized-minor-mode global-fci-mode fci-mode
      (lambda () (fci-mode 1)))
    (global-fci-mode 1))

  (when window-system
    ;; (require 'monokai-theme)
    ;; (require 'github-theme)
    ;; (require 'tango-2-theme)
    ;; (require 'burp-theme)
    (require 'google-code-theme)
    )
  )
(provide 'yesudeep)
