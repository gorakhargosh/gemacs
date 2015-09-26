;;; gemacs-c.el --- C-related stuff.
;;
;;; Commentary:
;;
;;; Code:

(require 'clang-format)
(require 'c-auto-include)

(add-hook 'c-mode-hook 'cwarn-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defun cfmt-before-save ()
  "Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'cfmt-before-save)."
  (interactive)
  (when (eq major-mode 'c-mode)
    (c-auto-include)
    (clang-format-buffer "Google")
    ))

(add-hook 'before-save-hook 'cfmt-before-save)

(provide 'gemacs-c)

;;; gemacs-c.el ends here
