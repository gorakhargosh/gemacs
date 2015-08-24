;;; gemacs-autocomplete.el --- Autocomplete related stuff.
;;
;;; Commentary:
;;
;;; Code:

(require 'smex)
(smex-initialize)
(setq smex-save-file "~/.emacs.d/.smex-items")
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'dabbrev)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"  ;; Our snippets.
        ;; Add more paths here if you like.
        ))
(yas-global-mode 1)  ;; or M-x yas/reload-all to reload yasnippet.

(require 'company)
(require 'company-go)
;; (require 'company-auctex)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-begin-commands '(self-insert-command))
(setq company-echo-delay 0)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 0)
(setq company-tooltip-limit 7)

;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (require 'go-autocomplete)
;; (ac-config-default)
;; (setq ac-ignore-case t
;;       ac-use-fuzzy t
;;       ac-auto-start 0
;;       ac-auto-show-menu 0.2
;;       ac-expand-on-auto-complete nil
;;       ac-dwim t)

;; ;; Use TAB to complete, not cycle.
;; (define-key ac-completing-map "\t" 'ac-complete)
;; ;; Disable RET completion.
;; (define-key ac-completing-map "\r" nil)
;; (define-key ac-completing-map [return] nil)

;; (setq ac-use-menu-map t)
;; (define-key ac-menu-map (kbd "C-n") 'ac-next)
;; (define-key ac-menu-map (kbd "C-p") 'ac-previous)

;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; ;; Set up sources for autocompletion.
;; (setq-default ac-sources
;;               '(ac-source-abbrev
;;                 ac-source-dictionary
;;                 ac-source-filename
;;                 ac-source-files-in-current-dir
;;                 ac-source-imenu
;;                 ;; ac-source-words-in-all-buffer
;;                 ac-source-words-in-buffer
;;                 ac-source-words-in-same-mode-buffers
;;                 ac-source-yasnippet
;;                 ))

;; ;; (global-auto-complete-mode t)
;; ;; Dirty fix to enable AC everywhere without bothering about the ac-modes list.
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                          (auto-complete-mode 1))
;;                        ))
;; (real-global-auto-complete-mode t)

(provide 'gemacs-autocomplete)

;;; gemacs-autocomplete.el ends here
