;;; gemacs-mouse.el --- Mouse-related configuration.
;;
;;; Commentary:
;;
;;; Code:

;; Scroll faster.
(setq mouse-wheel-scroll-amount '(7 ((shift) . 1) ((control) . nil)))

;; Enables mouse support scrolling when using Emacs in the terminal.
;; (unless window-system
;;   (require 'mouse)
;;   (xterm-mouse-mode t)
;;   (defun track-mouse (e))
;;   ;; (global-set-key [mouse-4] '(lambda ()
;;   ;;                              (interactive)
;;   ;;                              (scroll-down 1)))
;;   ;; (global-set-key [mouse-5] '(lambda ()
;;   ;;                              (interactive)
;;   ;;                              (scroll-up 1)))
;;   (setq mouse-sel-mode t)
;;   )

(provide 'gemacs-mouse)

;;; gemacs-mouse.el ends here
