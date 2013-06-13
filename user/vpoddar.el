(progn
  (setq user-full-name "Vivek Poddar"
        user-mail-address "vpoddar@google.com")
  (when window-system
    (require 'burp-theme)
    ;; (require 'cloud-theme)
    ;; (require 'google-code-theme)

    ;; Transparency.
    ;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
    (set-frame-parameter (selected-frame) 'alpha '(85 50))
    (add-to-list 'default-frame-alist '(alpha 85 50))
    )
  )
(provide 'vpoddar)
