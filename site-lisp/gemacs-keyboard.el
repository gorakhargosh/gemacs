;;; gemacs-keyboard.el --- Keyboard related stuff.
;;; Commentary:
;;; Code:

(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Shift region left or right.
(global-set-key (kbd "M-]") 'shift-right)
(global-set-key (kbd "M-[") 'shift-left)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Increase/decrease/reset font size.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-reset)

;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Killing and yanking.
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-<delete>") 'kill-word)

;; Line insertion
(global-set-key (kbd "S-<return>") 'insert-blank-line-below)
(global-set-key (kbd "M-S-<return>") 'insert-blank-line-above)
(global-set-key (kbd "s-<return>") 'insert-blank-line-below-next-line)

;; Line or region duplication.
(global-set-key (kbd "C-x C-d") 'duplicate-current-line-or-region)

;; Toggle identifier case.
(global-set-key (kbd "C-x t c") 'toggle-identifier-naming-style)

;; Comment what I really mean.
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; Sort lines
(global-set-key (kbd "C-c s") 'sort-lines)

;; Jump easily between beginning and end of defuns.
(global-set-key (kbd "s-<up>") 'beginning-of-defun)
(global-set-key (kbd "s-<down>") 'end-of-defun)

;; Parentheses matching.
(global-set-key (kbd "M-0") 'goto-match-paren)

;; Quotes.
(global-set-key (kbd "M-'") 'toggle-quotes)

;; Transpose parameters.
(global-set-key (kbd "s-t") 'transpose-params)

;; Switch buffer.
(global-set-key (kbd "C-x o") 'switch-window)

;; I don't use F2 much, so binding it here to highlight symbol.
(global-set-key [(control f2)] 'highlight-symbol-at-point)
(global-set-key [f2] 'highlight-symbol-next)
(global-set-key [(shift f2)] 'highlight-symbol-prev)
(global-set-key [(meta f2)] 'highlight-symbol-prev)

;; Search, search, search.
(global-set-key [(f5)] 'ack-and-a-half-find-file)
(global-set-key [(f7)] 'ack-and-a-half)
(global-set-key [(shift f7)] 'ack-and-a-half-same)
(global-set-key [(meta o)] 'ido-goto-symbol)     ;; Jump to symbol.

;; Org mode key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(key-chord-define-global "hj" 'undo)
(key-chord-define-global "jk" 'dabbrev-expand)
(key-chord-define-global ";'" 'ido-recentf-open)

(require 'multiple-cursors)
(global-set-key (kbd "M-(") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-)") 'mc/mark-next-like-this)
(global-set-key (kbd "M-\\") 'mc/mark-all-like-this-dwim)

(provide 'gemacs-keyboard)

;;; gemacs-keyboard.el ends here
