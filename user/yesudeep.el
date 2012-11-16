(progn
  (setq user-full-name "Yesudeep Mangalapilly"
        user-mail-address "yesudeep@google.com")

  (when window-system
    (when (goog/platform/is-linux-p)
      ;; (set-face-font 'default "Ubuntu Mono-18")
      ;; (set-face-font 'default "Consolas-14")
      (set-face-font 'default "Monaco-11"))
    ;; (require 'monokai-theme)
    ;; (require 'github-theme)
    ;; (require 'tango-2-theme)
    (require 'burp-theme)
    ;; (require 'cloud-theme)
    ;; (require 'google-code-theme)
    )

  (setq gnus-select-method '(nnimap "gmail"
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port 993)
                                    (nnimap-stream ssl)))
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "yesudeep@google.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-local-domain "google.com")
  (setq gnus-ignored-newsgroups "")

  ;; (require 'auto-async-byte-compile)
  ;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  ;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

  ;; Byte compilation.
  ;; (defun byte-compile-current-buffer ()
  ;;   "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
  ;;   (interactive)
  ;;   (when (and (eq major-mode 'emacs-lisp-mode)
  ;;              (file-exists-p (byte-compile-dest-file buffer-file-name)))
  ;;     (byte-compile-file buffer-file-name)))

  ;; (add-hook 'after-save-hook 'byte-compile-current-buffer)

  ;; ;; Test all the new stuff here before pushing to global.
  ;; (global-set-key [f5] 'slime-js-reload)
  ;; (add-hook 'js2-mode-hook
  ;;           (lambda ()
  ;;             (slime-js-minor-mode 1)))

  ;; (add-hook 'css-mode-hook
  ;;         (lambda ()
  ;;           (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
  ;;           (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))
  )
(provide 'yesudeep)
