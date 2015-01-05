;;; init.el -- Emacs configuration in a single file.
;;
;; Copyright (C) 2012 Google Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: yesudeep@google.com (Yesudeep Mangalapilly)
;;
;;; This file is NOT a part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'cl))

;; Dear Emacs, please don't make me wait at startup.
(modify-frame-parameters nil '((wait-for-wm . nil)))

(setq redisplay-dont-pause t)

(set-locale-environment "en_US.UTF-8")

;; ======================================================================
;; Initial configuration.
;; ======================================================================
;; (setq user-emacs-directory (file-name-directory (or (buffer-file-name)
;;                                           load-file-name)))
(add-to-list 'load-path user-emacs-directory)

;; Directory paths.
(setq goog-site-lisp-dir (expand-file-name
                          (concat user-emacs-directory "site-lisp")))

;; Load-path.
(add-to-list 'load-path goog-site-lisp-dir)

;; Add all subdirectories of site-lisp to load path.
(let ((default-directory goog-site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; Network-specific configuration is loaded from goog-network-dir.
(setq goog-network-name "corp.google.com"
      goog-network-dir (format "~/.%s/emacs.d/" goog-network-name)
      goog-network-re ".*[.]corp[.]google[.]com")


;; ======================================================================
;; Platform detection.
;; ======================================================================
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

;; ----------------------------------------------------------------------
;; goog/elisp
;; ----------------------------------------------------------------------
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


;; ----------------------------------------------------------------------
;; Define automatic mode detection for file types.
;; ----------------------------------------------------------------------
(setq auto-mode-alist
      (append '(
                ;; YAML.
                ("\\.yaml$" . yaml-mode)
                ("\\.yml" . yaml-mode)

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


;; Documentation and help.
(require 'eldoc) ; if not already loaded

;; Editing
(require 'gemacs-autocomplete)

;; Programming languages.
(require 'gemacs-clojure)
(require 'gemacs-css)
(require 'gemacs-dart)
(require 'gemacs-go)
(require 'gemacs-html)
(require 'gemacs-ielm)
(require 'gemacs-javascript)
(require 'gemacs-protobuf)
(require 'gemacs-python)
(require 'gemacs-sgml)
(require 'gemacs-sh)
(require 'gemacs-sql)
(require 'gemacs-tup)
(require 'gemacs-neo4j)

;; Tools.
(require 'gemacs-flycheck)
(require 'gemacs-git)

(require 'gemacs-keyboard)



;; ----------------------------------------------------------------------
;; Now load host, os, network-specific configuration.
;; ----------------------------------------------------------------------
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
