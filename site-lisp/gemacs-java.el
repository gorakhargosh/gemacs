;;; gemacs-java.el --- Java related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; You will need Eclipse installed on your system to take advantage of this.
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

;; See:http://stackoverflow.com/questions/19953924/how-do-you-run-java-codes-in-emacs
(defun goog/config/java-mode/java-eval-nofocus ()
  "run current program (that requires no input)"
  (interactive)
  (let* ((source (file-name-nondirectory buffer-file-name))
     (out    (file-name-sans-extension source))
     (class  (concat out ".class")))
    (save-buffer)
    (shell-command (format "rm -f %s && javac %s" class source))
    (if (file-exists-p class)
    (shell-command (format "java %s" out) "*scratch*")
      (progn
    (set (make-local-variable 'compile-command)
         (format "javac %s" source))
    (command-execute 'compile)))))

(add-hook 'java-mode-hook 'goog/config/java-mode/setup)

(defun goog/config/java-mode/setup ()
  "Configures `java-mode'."

  (setq c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil)
  (define-key java-mode-map (kbd "C-c C-e")
    'goog/config/java-mode/java-eval-nofocus))

(provide 'gemacs-java)

;;; gemacs-java.el ends here
