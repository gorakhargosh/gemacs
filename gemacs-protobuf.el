;;; gemacs-protobuf.el --- Protobuf related stuff.
;;
;;; Commentary:
;;
;;; Code:

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(defconst google-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "google-style" google-protobuf-style)))

(provide 'gemacs-protobuf)

;;; gemacs-protobuf.el ends here
