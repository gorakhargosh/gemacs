;;; gemacs-sgml.el --- SGML related stuff.
;;
;;; Commentary:
;;
;;; Code:

(require 'sgml-mode)
(autoload 'rename-sgml-tag "rename-sgml-tag" nil t)
(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

(provide 'gemacs-sgml)

;;; gemacs-sgml.el ends here
