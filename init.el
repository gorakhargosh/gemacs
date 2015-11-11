;;; init.el -- Emacs configuration in a single file. See COPYING for license.
;;; Commentary:
;;; Code:

;; FIXME(yesudeep): tmux eats up the C-y keyboard binding to cause yanking to
;; break within any session. The keyboard combination works fine outside any
;; active tmux sessions.

(package-initialize) ;; DO NOT delete.

(eval-when-compile
  (require 'cl))

;; Dear Emacs, please don't make me wait at startup.
(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq redisplay-dont-pause t)

(set-locale-environment "en_US.UTF-8")

(setq goog-site-lisp-dir (expand-file-name
                          (concat user-emacs-directory "site-lisp")))
(add-to-list 'load-path goog-site-lisp-dir)

;; Add all subdirectories of site-lisp to load path.
(let ((default-directory goog-site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Network-specific configuration is loaded from goog-network-dir.
(setq goog-network-name "corp.google.com"
      goog-network-dir (format "~/.%s/emacs.d/" goog-network-name)
      goog-network-re ".*[.]corp[.]google[.]com")

;; Platform detection.
(defun goog/platform/is-darwin-p ()
  "Determines whether the system is darwin-based (Mac OS X)"
  (interactive)
  (string-equal system-type "darwin"))

(defun goog/platform/is-linux-p ()
  "Determines whether the system is GNU/Linux-based."
  (interactive)
  (string-equal system-type "gnu/linux"))


(require 'gemacs-packages)

;; Copy environment variables on Mac OS X because Apple is an asshole
;; and breaks everything all the time.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'goog-defuns)
(require 'gemacs-appearance)
(require 'gemacs-workspace)
(require 'gemacs-editing)

(defun goog/elisp/load-current-module ()
  "Load the current Elisp module."
  (interactive)
  (load-file buffer-file-name))

(defun goog/elisp/load-directory (dir)
  "Load all elisp modules from the given DIR directory."
  (when (file-exists-p dir)
    (mapc 'load (directory-files dir t "^[^#].*el"))))

(defun goog/elisp/load-if-exists (name)
  "Load an elisp with the given NAME module if it exists."
  (when (file-exists-p name)
    (load name)))

(defun goog/elisp/reload-configuration ()
  "Reloads the init.el module from the Emacs configuration directory."
  (interactive)
  ;; (load-file (concat user-emacs-directory "init.el"))
  (load-file (concat user-emacs-directory "init.el"))
  (yas-reload-all))

(defun goog/elisp/eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM
immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(setq auto-mode-alist
      (append '(
                ;; YAML.
                ("\\.raml" . yaml-mode)
                ("\\.yaml$" . yaml-mode)
                ("\\.yaml$" . yaml-mode)

                ;; CSS.
                ("\\.gss" . css-mode)

                ;; Sass.
                ("\\.sass" . sass-mode)
                ("\\.scss" . sass-mode)

                ;; HTML.
                ("\\.tmpl" . html-mode)  ;; Server-side template extension.
                ("\\.mustache" . html-mode)  ;; Mustache template extension.
                ("\\.ng" . html-mode)    ;; Angular templates.

                ;; Dart lang.
                ("\\.dart$" . dart-mode)

                ;; Python.
                ("\\BUCK$" . python-mode)
                ("\\BUILD$" . python-mode)
                ("\\SConscript" . python-mode)
                ("\\SConstruct" . python-mode)
                ("\\wscript$" . python-mode)

                ;; JavaScript.
                ("\\.js$" . js2-mode)
                ("\\.json$" . js2-mode)

                ;; Less CSS mode.
                ("\\.less\\'" . less-css-mode)

                ;; Markdown
                ("\\.text\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)
                ("\\.md\\'" . markdown-mode)

                ;; SQL mode.
                ;; ("\\.mysql$" . sql-mode)

                ;; Configuration files.
                ("\\(?:\\.gitconfig\\|\\.gitmodules\\|config\\)$" . conf-mode)
                )
              auto-mode-alist))

(require 'eldoc)
(require 'gemacs-autocomplete)

(require 'gemacs-c)
(require 'gemacs-clojure)
(require 'gemacs-css)
(require 'gemacs-go)
(require 'gemacs-haskell)
(require 'gemacs-html)
(require 'gemacs-ielm)
(require 'gemacs-javascript)
(require 'gemacs-neo4j)
(require 'gemacs-protobuf)
(require 'gemacs-python)
(require 'gemacs-sgml)
(require 'gemacs-sh)
(require 'gemacs-sql)
(require 'gemacs-tex)
(require 'gemacs-tup)

(require 'gemacs-flycheck)
(require 'gemacs-git)
(require 'gemacs-keyboard)
(require 'gemacs-chromebook)

;; Java has the worst support in Emacs. If any of this fails, quite a lot of
;; stuff starts breaking. Load this last.
(require 'gemacs-java)


(progn
  (message "Loading user, network, and host-specific configuration.")

  ;; ~/.emacs.d/*
  (setq goog-local-hosts-dir (concat user-emacs-directory "host/"))
  (setq goog-local-users-dir (concat user-emacs-directory "user/"))
  (setq goog-local-os-dir (concat user-emacs-directory "os/"))

  ;; ~/.example.com/*.
  (setq goog-network-hosts-dir (concat goog-network-dir "host/"))
  (setq goog-network-users-dir (concat goog-network-dir "user/"))

  ;; ~/.emacs.d/hosts/dhcp-172-26-239-50.hyd.corp.google.com.el
  (setq goog-hostname-config (concat goog-local-hosts-dir system-name ".el"))

 ;; ~/.emacs.d/users/<username>.el
 (setq goog-username-config (concat goog-local-users-dir user-login-name ".el"))

 ;; ~/.emacs.d/hosts/dhcp-172-26-239-50.hyd.corp.google.com/*.el
 (setq goog-hostname-dir (concat goog-local-hosts-dir system-name))

 ;; ~/.emacs.d/users/<username>/*.el
 (setq goog-username-dir (concat goog-local-users-dir user-login-name))

 ;; ~/.emacs.d/os/<os-type>/*.el
 ;; TODO(yesudeep): os-type needs to be normalized from system-type. For
 ;; example, Linux is represented by "gnu/linux" which cannot be a reliably
 ;; valid directory name.
 (setq goog-os-dir (concat goog-local-os-dir (format "%s" system-type)))

 (message "Setting user configuration.")
 (goog/elisp/eval-after-init
  '(progn
     (message "Network: %s" goog-network-name)
     (message "Hostname: %s" system-name)
     (message "Username: %s" user-login-name)

     ;; --------------------------------------------------
     ;; Network wide.
     ;; --------------------------------------------------
     (when (string-match goog-network-re system-name)
       (goog/elisp/load-directory goog-network-dir)
       (goog/elisp/load-directory goog-network-hosts-dir)
       (goog/elisp/load-directory goog-network-users-dir))

     ;; --------------------------------------------------
     ;; Local
     ;; --------------------------------------------------
     (goog/elisp/load-if-exists goog-hostname-config)
     (goog/elisp/load-if-exists goog-username-config)

     (goog/elisp/load-directory goog-hostname-dir)
     (goog/elisp/load-directory goog-username-dir)
     (goog/elisp/load-directory goog-os-dir)

     )
   ))

;;; init.el ends here
