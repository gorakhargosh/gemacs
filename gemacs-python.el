;;; gemacs-python.el --- Python related stuff.
;;
;;; Commentary:
;;
;;; Code:

(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook 'goog/config/python-mode/setup)
(font-lock-add-keywords
 'python-mode
 '(;; Adds object and str and fixes it so that keywords that often appear
   ;; with : are assigned as builtin-face
   ("\\<\\(object\\|str\\|else\\|except\\|finally\\|try\\|\\)\\>" 0 py-builtins-face)
   ;; FIXME: negative or positive prefixes do not highlight to this regexp
   ;; but does to one below
   ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-constant-face)
   ("\\([][{}()~^<>:=,.\\+*/%-]\\)" 0 'widget-inactive-face)))

;; (eval-after-load "company"
;;   '(progn
;;      (add-to-list 'company-backends 'company-anaconda)))
;; (add-hook 'python-mode-hook 'anaconda-mode)

(defun goog/config/python-mode/setup ()
  "Configures `python-mode'."
  (setq indent-tabs-mode nil
        require-final-newline 't
        tab-width 2
        python-indent-offset 2
        ;; python-indent 2
        py-indent-offset 2)

  ;; Auto-complete configuration.
  ;; (setq ac-auto-start 0)
  )

(provide 'gemacs-python)

;;; gemacs-python.el ends here
