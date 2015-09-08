;;; gemacs-editing.el --- Editing related stuff.
;;; Commentary:
;;; Code:

;; Editing and searching.
(setq-default fill-column 80)     ;; right margin and fill column.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-watchwords)
(add-hook 'prog-mode-hook 'turn-on-watchwords)

(defun goog/prog-mode-common/setup ()
  "Applies settings common to all programming language modes."
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face)))))
(add-hook 'prog-mode-common-hook 'goog/prog-mode-common/setup)

;; Use whitespace mode. Cleans up trailing spaces, shows tabs, unnecessary
;; whitespace, EOF newline, bad indentation, margin bleeds, etc.
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style
      '(face empty indentation lines-tail newline trailing)
      whitespace-action '(auto-cleanup warn-read-only)
      whitespace-line-column 80)
;; Disabled space-mark newline-mark because it makes code very hard to read.
(setq whitespace-display-mappings
      '(
        (space-mark   ?\    [?\xB7]     [?.])       ;; space
        (space-mark   ?\xA0 [?\xA4]     [?_])       ;; hard space
        (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])   ;; end-of-line
        ))

(require 're-builder)
(setq reb-re-syntax 'string)

;; Configure pairs of characters and parenthese matching.
(require 'smartparens)
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(progn
  (show-paren-mode t)                ;; Highlight matching parentheses.
  (smartparens-global-mode t)
  (show-smartparens-global-mode +1)

  ;; Don't automatically escape quotes within quotes.
  (setq sp-autoescape-string-quote nil)
  )

;; Clipboard and kill-ring.
(setq x-select-enable-clipboard t) ;; <3 clipboard copy-paste.
;; https://github.com/bbatsov/emacs-prelude/commit/d26924894b31d5dc3a8b2813719579baccc2b433
(when (goog/platform/is-darwin-p)
  (defun goog/clipboard/copy-from-osx ()
    "Pastes from the OS X clipboard."
    (shell-command-to-string "pbpaste"))
  (defun goog/clipboard/paste-to-osx (text &optional push)
    "Copies text into the OS X clipboard."
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'goog/clipboard/paste-to-osx
        interprogram-paste-function 'goog/clipboard/copy-from-osx))

;; SlickCopy. Makes the kill commands work on the line if nothing is selected.
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

;; Whitespace-aware kill-line.
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Cleans up whitespace on `kill-line'."
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; Undo and history.
(require 'undo-tree)
(require 'move-lines)
(move-lines-binding)
(autoload 'expand-region "expand-region" nil t)
(autoload 'contract-region "expand-region" nil t)
(global-set-key (kbd "M-8") 'er/expand-region)
(global-set-key (kbd "M-7") 'er/contract-region)

(require 'ace-jump-mode)
(define-key global-map (kbd "M-2") 'ace-jump-mode)

(require 'fastnav)
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
(global-set-key "\M-r" 'fastnav-replace-char-forward)
(global-set-key "\M-R" 'fastnav-replace-char-backward)
(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
(global-set-key "\M-k" 'fastnav-delete-char-forward)
(global-set-key "\M-K" 'fastnav-delete-char-backward)
(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
(global-set-key "\M-p" 'fastnav-sprint-forward)
(global-set-key "\M-P" 'fastnav-sprint-backward)

;; Find things faster.
(require 'fafafa)
(defvar fafafa-filetypes
 '("*")
 "A list of filetype patterns that grepsource will use.")
(autoload 'fafafa-find-file "find-things-fast" nil t)
(autoload 'fafafa-grepsource "find-things-fast" nil t)
(global-set-key (kbd "C-x f") 'fafafa-find-file)
(global-set-key (kbd "C-x M-f") 'fafafa-grepsource)

;; Highlight current symbol.
(require 'highlight-symbol)
(defun goog/config/highlight-symbol-mode/setup ()
  (when window-system
    (highlight-symbol-mode)
    (setq highlight-symbol-idle-delay 0.1)
    ))
(add-hook 'text-mode-hook 'goog/config/highlight-symbol-mode/setup)
(add-hook 'prog-mode-hook 'goog/config/highlight-symbol-mode/setup)
;; Why does js2-mode not inherit from prog-mode?
(add-hook 'js2-mode-hook 'goog/config/highlight-symbol-mode/setup)

;; global diff highlight mode.
(global-diff-hl-mode)

(provide 'gemacs-editing)

;;; gemacs-editing.el ends here
