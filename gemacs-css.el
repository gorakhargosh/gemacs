;;; gemacs-css.el --- CSS related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; (require 'less-css-mode)
(defun goog/config/css-mode/setup ()
  "Configures `css-mode'."

  ;; Coding style.
  (setq less-css-indent-level 2
        css-indent-offset 2
        indent-tabs-mode nil
        require-final-newline 't
        tab-width 2)

  ;; Autocomplete.
  (setq ac-sources '(
                     ac-source-words-in-buffer
                     ac-source-abbrev
                     ac-source-symbols
                     ac-source-variables
                     ac-source-words-in-same-mode-buffers
                     ac-source-yasnippet
                     ac-source-css-property
                     )))
(add-hook 'css-mode-hook 'goog/config/css-mode/setup)
;; (add-hook 'less-css-mode-hook 'goog/config/css-mode/setup)

(provide 'gemacs-css)

;;; gemacs-css.el ends here
