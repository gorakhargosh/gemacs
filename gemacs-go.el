;;; gemacs-go.el --- Golang related stuff.
;;
;;; Commentary:
;;
;;; Code:

(autoload 'go-mode "go-mode" nil t)
(eval-after-load 'go-mode
  '(when (executable-find "goimports")
     (setq gofmt-command "goimports")))

;; Documentation.
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

(defun goog/config/go-mode/execute-buffer ()
  "Formats, compiles and executes the Go code in the current buffer."
  (interactive)
  (gofmt)
  (save-buffer)
  (compile (concat "go run " buffer-file-name)))

(defun goog/config/go-mode/build-buffer ()
  "Formats and compiles the Go code in the current buffer."
  (interactive)
  (gofmt)
  (save-buffer)
  (compile (concat "go build " buffer-file-name)))

(defun goog/config/go-mode/setup ()
  "Configures `go-mode'."
  ;; Set up coding style.
  (setq tab-width 2)

  ;; Keyboard bindings.
  (define-key go-mode-map (kbd "C-c l f") 'gofmt)
  (define-key go-mode-map (kbd "C-x C-e") 'goog/config/go-mode/execute-buffer)
  (define-key go-mode-map (kbd "C-c C-e") 'goog/config/go-mode/execute-buffer)
  (define-key go-mode-map (kbd "C-c C-b") 'goog/config/go-mode/build-buffer)
  ; Godef jump key binding
  (define-key go-mode-map (kbd "C-c C-j") 'godef-jump)
  )

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 'goog/config/go-mode/setup)

(provide 'gemacs-go)

;;; gemacs-go.el ends here
