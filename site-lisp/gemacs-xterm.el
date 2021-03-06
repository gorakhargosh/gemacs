;;; gemacs-xterm.el --- Xterm related configuration.
;;
;;; Commentary:
;;
;;; Code:

;; Scroll faster.
(setq mouse-wheel-scroll-amount '(7 ((shift) . 1) ((control) . nil)))

(defun fix-up-xterm-control-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])))


;; (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
;; (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

;; (defun goog/console-frame-setup ()
;;   (when (< emacs-major-version 23)
;;     )
;;   (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
;;   (when (fboundp 'mwheel-install)
;;     (mwheel-install)))

(defun goog/console-frame-setup ()
  (fix-up-xterm-control-arrows))

(add-hook 'after-make-console-frame-hooks 'goog/console-frame-setup)

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
;;   ;; (setq mouse-sel-mode t)
;;   )

(provide 'gemacs-xterm)

;;; gemacs-xterm.el ends here
