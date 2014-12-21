;;; gemacs-flycheck.el --- Flycheck related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Static analysis.
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-hook 'prog-mode-hook 'flycheck-mode)
;; (add-hook 'go-mode-hook 'flycheck-mode)
;; (add-hook 'text-mode-hook 'flycheck-mode)

(provide 'gemacs-flycheck)

;;; gemacs-flycheck.el ends here
