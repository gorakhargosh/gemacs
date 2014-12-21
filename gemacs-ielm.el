;;; gemacs-ielm.el --- IELM related stuff.
;;
;;; Commentary:
;;
;;; Code:

(defun goog/config/ielm-mode/setup ()
  "Configures \\[ielm]."

  ;; Autocomplete.
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  ;; (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))

(add-hook 'ielm-mode-hook 'goog/config/ielm-mode/setup)

(provide 'gemacs-ielm)

;;; gemacs-ielm.el ends here
