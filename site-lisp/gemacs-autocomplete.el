;;; gemacs-autocomplete.el --- Autocompletion.
;;; Commentary:
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
(add-hook 'after-init-hook 'global-company-mode)

(setq company-begin-commands '(self-insert-command))
(setq company-echo-delay 0)
(setq company-idle-delay .3)
(setq company-minimum-prefix-length 0)
(setq company-tooltip-limit 7)

(provide 'gemacs-autocomplete)
;;; gemacs-autocomplete.el ends here
