;;; gemacs-git.el --- Git related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Git integration.
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load "magit"
  '(progn
     (setq magit-completing-read-function 'magit-ido-completing-read)
     ))

(provide 'gemacs-git)

;;; gemacs-git.el ends here
