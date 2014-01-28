(progn
  (setq user-full-name "Pranav Hegde"
        user-mail-address "mphegde@google.com")
  (when window-system
    ;; ;; Transparency. Set this before setting up your theme. The theme may
    ;; ;; configure itself to use better colors for transparency.
    ;; ;;(set-frame-parameter (selected-frame) 'alpha '(<aemctive> [<inactive>]))
    ;; (set-frame-parameter (selected-frame) 'alpha '(85 70))
    ;; (add-to-list 'default-frame-alist '(alpha 85 70))
    (require 'burp-theme)
    ))

(provide 'mphegde)
