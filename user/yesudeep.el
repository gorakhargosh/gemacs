;;; Package -- User configuration for yesudeep.
;;;
;;; Commentary:

;;; Code:

(progn
  (setq user-full-name "Yesudeep Mangalapilly"
        user-mail-address "yesudeep@google.com")

  (when window-system
    ;; Transparency. Set this before setting up your theme. The theme may
    ;; configure itself to use better colors for transparency.
    ;; (set-frame-parameter (selected-frame) 'alpha '(<aemctive> [<inactive>]))
    (set-frame-parameter (selected-frame) 'alpha '(95 85))
    (add-to-list 'default-frame-alist '(alpha 95 85))

    ;; Now set the default theme.
    ;; (require 'burp-theme)
    ;; (require 'cloud-theme)
    ;; (require 'github-theme)
    ;; (require 'google-code-theme)
    ;; (require 'monokai-theme)
    ;; (require 'tango-2-theme)
    ;; (require 'tangotango-theme)
    ;; (require 'cloud-theme)
    ;; (powerline-default-theme)

    ;; Transparency.
    ;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
    ;; (set-frame-parameter (selected-frame) 'alpha '(85 80))
    ;; (add-to-list 'default-frame-alist '(alpha 85 80))
    )
  )

(provide 'yesudeep)

;;; yesudeep.el ends here
