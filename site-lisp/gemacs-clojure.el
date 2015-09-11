;;; gemacs-clojure.el -- Clojure configuration.
;;; Commentary:
;;; Code:

(progn
 (add-hook 'cider-mode-hook #'eldoc-mode)
 (setq cider-auto-mode 1)
 )

(provide 'gemacs-clojure)

;;; gemacs-clojure.el ends here
