;;; gemacs-sh.el --- SH related stuff.
;;
;;; Commentary:
;;
;;; Code:

(defun goog/config/sh-mode/setup ()
  "Configures `sh-mode'."

  ;; Coding style.
  (setq sh-basic-offset 2
        sh-indentation 2))

(add-hook 'sh-mode-hook 'goog/config/sh-mode/setup)

(provide 'gemacs-sh)

;;; gemacs-sh.el ends here
