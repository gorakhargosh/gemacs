;;; gemacs-wprkspace.el --- Workspace related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Use unique buffer names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `((".*" . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves" t)))

(provide 'gemacs-wprkspace)

;;; gemacs-wprkspace.el ends here
