;;; gemacs-javascript.el --- JavaScript related stuff.
;;
;;; Commentary:
;;
;;; Code:

(autoload 'js2-mode "js2-mode" nil t)
(eval-after-load "js2-mode"
  '(progn
     (add-hook 'js2-mode-hook 'goog/config/js-mode/setup)
     (add-hook 'js2-mode-hook
               '(lambda ()
                  (font-lock-add-keywords nil font-lock-operator-keywords t))
               t t)
     ))
(add-hook 'js-mode-hook 'goog/config/js-mode/setup)

(defun goog/config/js-mode/setup ()
  "Configures `js-mode' and `js2-mode'."

  ;; Coding style.
  (setq js-indent-level 2
        espresso-indent-level 2
        c-basic-offset 2
        js2-basic-offset 2
        js2-indent-on-enter-key nil
        js2-enter-indents-newline t
        js2-mode-squeeze-spaces nil
        ;; js2-bounce-indent-p t
        js2-auto-indent-p t)

  ;; Keyboard bindings.
  (define-key js2-mode-map (kbd "C-c l l")
    'goog/config/js-mode/gjslint-buffer)
  (define-key js2-mode-map (kbd "C-c l d")
    'goog/config/js-mode/gjslint-dir)
  (define-key js2-mode-map (kbd "C-c l D")
    'goog/config/js-mode/fixjsstyle-dir)
  (define-key js2-mode-map (kbd "C-c l f")
    'goog/config/js-mode/fixjsstyle-buffer)
  (define-key js2-mode-map (kbd "C-c l F")
    'goog/config/js-mode/fixjsstyle-buffer-compile)
  (define-key js2-mode-map (kbd "C-c l h")
    'goog/config/js-mode/jshint)

  ;; Keychords.
  (key-chord-define js2-mode-map ";;"  "\C-e;")
  (key-chord-define js2-mode-map ",,"  "\C-e,")

  (js2r-add-keybindings-with-prefix "C-c C-m"))


;; Tools for Javascript.
(defun goog/config/js-mode/gjslint-buffer ()
  "Runs gjslint in strict mode on the current buffer."
  (interactive)
  (compile (concat "gjslint --strict --unix_mode --custom_jsdoc_tags=ngInject,notypecheck --closurized_namespaces=appkit,bulletin,carbon,cg,croc,goa,goog,hub,jfk,md,mountie,northstar,ooo,shub,tvt " buffer-file-name)))

(defun goog/config/js-mode/gjslint-dir ()
  "Runs gjslint in strict mode on the parent directory of the file in the
current buffer."
  (interactive)
  (compile (concat "gjslint --strict --unix_mode --custom_jsdoc_tags=ngInject,notypecheck --closurized_namespaces=appkit,bulletin,carbon,cg,croc,goa,goog,hub,jfk,md,mountie,northstar,ooo,shub,tvt -r "
                   (file-name-directory buffer-file-name))))

(defun goog/config/js-mode/fixjsstyle-buffer ()
  "Runs fixjsstyle in strict mode on the buffer."
  (interactive)
  (shell-command (concat "fixjsstyle --strict " buffer-file-name)))

(defun goog/config/js-mode/fixjsstyle-dir ()
  "Runs fixjsstyle in strict mode on the parent directory of the file in
the current buffer."
  (interactive)
  (compile (concat "fixjsstyle --strict -r " (file-name-directory
                                              buffer-file-name))))

(defun goog/config/js-mode/fixjsstyle-buffer-compile ()
  "Runs fixjsstyle in strict mode on the current buffer and shows
compilation output."
  (interactive)
  (compile (concat "fixjsstyle --strict " buffer-file-name)))

(defun goog/config/js-mode/jshint-buffer ()
  "Runs jshint on the current buffer."
  (interactive)
  (compile (concat "jshint " buffer-file-name)))

(provide 'gemacs-javascript)

;;; gemacs-javascript.el ends here
