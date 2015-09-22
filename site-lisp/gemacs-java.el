;;; gemacs-java.el --- Java related stuff.
;;
;;; Commentary:
;;
;;; Code:

(require 'eclim)
(global-eclim-mode)

(require 'eclimd)

(require 'company-emacs-eclim)
(company-emacs-eclim-setup)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(setq company-emacs-eclim-ignore-case t)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))

(provide 'gemacs-java)

;;; gemacs-java.el ends here
