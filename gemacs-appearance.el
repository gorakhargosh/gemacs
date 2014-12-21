;;; gemacs-appearance.el --- Appearance related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Additional faces.
(defvar font-lock-operator-face 'font-lock-operator-face)
(defface font-lock-operator-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "dark red"))
    (t nil))
  "Used for operators."
  :group 'font-lock-faces)
(defvar font-lock-end-statement-face 'font-lock-end-statement-face)
(defface font-lock-end-statement-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "DarkSlateBlue"))
    (t nil))
  "Used for end statement symbols."
  :group 'font-lock-faces)
(defvar font-lock-operator-keywords
  '(("\\([][|!.+=&/%*,<>(){}:^~-]+\\)" 1 font-lock-operator-face)
    (";" 0 font-lock-end-statement-face)))


;; Now set the default theme.
(require 'golokai-theme)
(powerline-default-theme)

(provide 'gemacs-appearance)

;;; gemacs-appearance.el ends here
