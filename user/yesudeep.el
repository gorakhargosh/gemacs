;;; Package -- User configuration for yesudeep.
;;;
;;; Commentary:

;;; Code:

(progn
  (setq user-full-name "Yesudeep Mangalapilly"
        user-mail-address "yesudeep@google.com")

  (when window-system
    ;; (when (goog/platform/is-linux-p)
    ;;   ;; (set-face-font 'default "Ubuntu Mono-18")
    ;;   ;; (set-face-font 'default "Consolas-14")
    ;;   (set-face-font 'default "Monaco-10")
    ;;   )
    ;; (when (goog/platform/is-darwin-p)
    ;;   (set-face-font 'default "Monaco-13")
    ;;   ;; (set-face-font 'default "Droid Sans Mono-13")
    ;;   )

    ;; Transparency. Set this before setting up your theme. The theme may
    ;; configure itself to use better colors for transparency.
    ;;(set-frame-parameter (selected-frame) 'alpha '(<aemctive> [<inactive>]))
    ;; (set-frame-parameter (selected-frame) 'alpha '(85 70))
    ;; (add-to-list 'default-frame-alist '(alpha 85 70))

    ;; (require 'monokai-theme)
    ;; (require 'github-theme)
    ;; (require 'tango-2-theme)
    ;; (require 'burp-theme)
    ;; (require 'cloud-theme)
    ;; (require 'google-code-theme)

    ;; Transparency.
    ;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
    ;; (set-frame-parameter (selected-frame) 'alpha '(85 80))
    ;; (add-to-list 'default-frame-alist '(alpha 85 80))
    )
  )

(provide 'yesudeep)

;;; yesudeep ends here
