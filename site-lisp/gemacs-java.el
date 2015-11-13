;;; gemacs-java.el --- Java related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; See: http://xiaohanyu.me/oh-my-emacs/modules/ome-java.html

(require 'eclim)
(global-eclim-mode)

(require 'eclimd)


(when (goog/platform/is-darwin-p)
  (add-to-list 'eclim-eclipse-dirs
               "/Applications/Eclipse.app/Contents/Eclipse")

  (setq eclim-executable (or (executable-find "eclim") "/Applications/Eclipse.app/Contents/Eclipse/eclim")
        eclimd-executable (or (executable-find "eclimd") "/Applications/Eclipse.app/Contents/Eclipse/eclimd")))

;; TODO(yesudeep): Separate google-specific and otherwise configuration.
(when (goog/platform/is-linux-p)
  (add-to-list 'eclim-eclipse-dirs
               (format "/usr/local/google/eclipse45_%s/stable/" user-login-name))

  (setq eclim-executable (or (executable-find "eclim") (format "/usr/local/google/eclipse45_%s/stable/eclim" user-login-name))
        eclimd-executable (or (executable-find "eclimd") (format "/usr/local/google/eclipse45_%s/stable/eclimd" user-login-name))))

(setq eclimd-wait-for-process nil
      eclimd-default-workspace "~/workspace/"
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1)

;; Do not enable these.
;; (setq eclim-auto-save t)
;; (setq eclim-print-debug-messages t)

;; Call the help framework with the settings above & activate
;; eclim-mode.
(help-at-pt-set-timer)

(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(setq company-emacs-eclim-ignore-case t)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)
                            (eclim-mode t)
                            ;; Whitespace mode does not work well with completion in java mode with eclim.
                            (set (make-local-variable 'whitespace-action) nil)
                            ))


(provide 'gemacs-java)

;;; gemacs-java.el ends here
