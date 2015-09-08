;;; gemacs-workspace.el --- Workspace related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; Use unique buffer names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `((".*" . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves" t)))

;; Autocompilation.
;; (add-to-list 'load-path user-emacs-directory)
;; (require 'auto-compile)
;; (auto-compile-on-load-mode 1)
;; (auto-compile-on-save-mode 1)

(require 'switch-window)

(require 'persp-mode)
(persp-mode t)

;; (require 'helm-config)
;; (helm-mode 1)

(autoload 'nav "nav" nil t)
(global-set-key (kbd "C-x C-a") 'nav)

(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; ----------------------------------------------------------------------
;; File and directory navigation.
;; ----------------------------------------------------------------------
(defun ibuffer-ido-find-file ()
  "`ido-find-file', but default to directory of buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

(setq find-file-wildcards t)
(eval-after-load "ido"
  '(progn
     (ido-mode t)
     (eval-after-load "ido-ubiquitous"
       '(progn
          (ido-ubiquitous-mode t)
          ))
     (setq ido-save-directory-list-file (concat user-emacs-directory ".ido.last")
           ido-use-filename-at-point 'guess
           ido-enable-flex-matching t
           confirm-nonexistent-file-or-buffer nil
           ido-create-new-buffer 'always)

     ;; Display ido results vertically, rather than horizontally.
     ;; From the Emacs wiki.
     (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
     (defun goog/config/ido-mode/disable-line-truncation ()
       (set (make-local-variable 'truncate-lines) nil))
     (add-hook 'ido-minibuffer-setup-hook 'goog/config/ido-mode/disable-line-truncation)
     (global-set-key [(control tab)] 'ido-switch-buffer)

     (defun goog/config/ido-mode/cycle-with-up-and-down-arrow-keys ()
       "Allow the up and down arrow keys to cycle through `ido-mode' results."
       (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
       (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
       )
     (add-hook 'ido-setup-hook 'goog/config/ido-mode/cycle-with-up-and-down-arrow-keys)
     ))

;; ----------------------------------------------------------------------
;; Finding files, recent files and sessions.
;; ----------------------------------------------------------------------
(autoload 'recentf "recentf" t)
(setq recentf-auto-cleanup 'never) ;; Disable before we start recentf for tramp.
(recentf-mode t)
(setq recentf-max-saved-items 400)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


(provide 'gemacs-workspace)

;;; gemacs-workspace.el ends here
