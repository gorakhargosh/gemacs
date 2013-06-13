(progn
  (setq user-full-name "Mohan Lal"
        user-mail-address "mohanl@google.com")
  (when window-system
    ;; (require 'monokai-theme)
    ;; (require 'github-theme)
    ;; (require 'tango-2-theme)
    (require 'burp-theme)

    ;; Transparency.
    ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
    (set-frame-parameter (selected-frame) 'alpha '(85 80))
    (add-to-list 'default-frame-alist '(alpha 85 80))
    )
  )
(provide 'mohanl)
