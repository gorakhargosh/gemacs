;;; gemacs-java.el --- Java related stuff.
;;
;;; Commentary:
;;
;;; Code:

(add-hook 'js-mode-hook 'goog/config/java-mode/setup)

(defun goog/config/java-mode/setup ()
  "Configures `java-mode'."

  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil))

(provide 'gemacs-java)

;;; gemacs-java.el ends here
