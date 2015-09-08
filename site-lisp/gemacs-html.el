;;; gemacs-html.el --- HTML related stuff.
;;
;;; Commentary:
;;
;;; Code:

(defun goog/config/html-mode/setup ()
  "Configures `html-mode'."

  ;; Coding style.
  (set (make-local-variable 'sgml-basic-offset) 2)
  (setq indent-tabs-mode nil)  ;; Don't use tabs to indent.

  ;; Autocompletion.
  (zencoding-mode 1))

(add-hook 'html-mode-hook 'goog/config/html-mode/setup)

(provide 'gemacs-html)

;;; gemacs-html.el ends here
