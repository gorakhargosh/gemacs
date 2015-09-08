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

;; Automatically set executable permissions on executable script files.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(provide 'gemacs-sh)

;;; gemacs-sh.el ends here
