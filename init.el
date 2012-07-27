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

(eval-when-compile
  (require 'cl))

;; Dear Emacs, please don't make me wait at startup.
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; ======================================================================
;; Initial configuration.
;; ======================================================================
(setq config-dir (file-name-directory (or (buffer-file-name)
                                          load-file-name)))
(add-to-list 'load-path config-dir)

;; Directory paths.
(setq goog-site-lisp-dir (expand-file-name
                          (concat config-dir "site-lisp")))

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
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

(defun goog/platform/is-linux-p ()
  (interactive)
  "Return true if system is GNU/Linux-based."
  (string-equal system-type "gnu/linux"))


;; ======================================================================
;; Package management.
;; ======================================================================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar default-packages '(
                           ;; fill-column-indicator
                           ;; flex-isearch
                           ;; smex   ;; Don't use this one. el-get works.
                           ;; smooth-scroll
                           auto-complete
                           autopair
                           expand-region
                           fastnav
                           find-things-fast
                           helm
                           ido-ubiquitous
                           iedit
                           js2-mode
                           key-chord
                           magit
                           mark-multiple
                           melpa
                           move-text
                           nav
                           paredit
                           perspective
                           switch-window
                           ;; tabbar  ;; This looks like shit.
                           undo-tree
                           yasnippet
                           zencoding-mode

                           ;; Themes.
                           django-theme
                           github-theme
                           inkpot-theme
                           ir-black-theme
                           ir_black-theme
                           monokai-theme
                           tango-2-theme
                           tron-theme
                           twilight-theme
                           ujelly-theme
                           underwater-theme
                           zenburn-theme
                           ))
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ----------------------------------------------------------------------
;; Installs el-get.
;; ----------------------------------------------------------------------
;; El-get manages packages for GNU Emacs and can install, load, configure
;; and uninstall packages for you easily.
;;
;; @see http://github.com/dimitri/el-get/
;; @see http://www.emacswiki.org/emacs/el-get
;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

;; ----------------------------------------------------------------------
;; El-get packages.
;; ----------------------------------------------------------------------
;; NOTE: Use el-get packages only if we do not have stable packages
;; in elpa,melpa,marmalade.
;; ----------------------------------------------------------------------
(setq
 el-get-sources
 '(el-get

   (:name smex              ;; a better (ido-like) M-x
          :after (progn
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   ;; (:name magit             ;; Git for emacs.
   ;;        :after (progn
   ;;                 (global-set-key (kbd "C-x g") 'magit-status)))

   ;; (:name move-text         ;; text movement
   ;;        :after (progn
   ;;                 (global-set-key [M-S-up] 'move-text-up)
   ;;                 (global-set-key [M-S-down] 'move-text-down)))

   ;; (:name helm              ;; You need this.
   ;;        :after (progn
   ;;                 (require 'helm-config)
   ;;                 (helm-mode 1)
   ;;                 (global-set-key (kbd "<f5>") 'helm-for-files)))

   ;; (:name autopair          ;; Balance parentheses; paredit is faster.
   ;;        :type git
   ;;        :url "git://github.com/capitaomorte/autopair.git"
   ;;        :after (progn
   ;;                 (require 'autopair)
   ;;                 (autopair-global-mode)
   ;;                 (setq autopair-autowrap t)

   ;;                 ;; Prevents: http://code.google.com/p/autopair/issues/detail?id=32
   ;;                 (add-hook 'sldb-mode-hook
   ;;                           #'(lambda ()
   ;;                               (setq autopair-dont-activate t) ;; emacs < 24
   ;;                               (autopair-mode -1)              ;; emacs >= 24
   ;;                               ))
   ;;                 (set-default 'autopair-dont-activate
   ;;                              #'(lambda ()
   ;;                                  (eq major-mode 'sldb-mode)))
   ;;                 ))

   ;; (:name emacs-nav
   ;;        :type hg
   ;;        :website "http://code.google.com/p/emacs-nav/"
   ;;        :description "Emacs Navigation Tool."
   ;;        :url "https://code.google.com/p/emacs-nav/"
   ;;        :after (progn
   ;;                 (require 'nav)))

   ;; (:name iedit             ;; More interactive find and replace.
   ;;        :after (progn
   ;;                 (require 'iedit)
   ;;                 (put 'narrow-to-region 'disabled nil)))

   ;; (:name paredit           ;; Automatically balance parentheses.
   ;;        :after (progn
   ;;                 (require 'goog-config-paredit-mode)))

   ;; (:name transpose-frame
   ;;        :after (progn
   ;;                 (require 'transpose-frame)
   ;;                 (global-set-key (kbd "C-x t t") 'transpose-frame)
   ;;                 (global-set-key (kbd "C-x t h") 'flop-frame)
   ;;                 (global-set-key (kbd "C-x t v") 'flip-frame)
   ;;                 (global-set-key (kbd "C-x t r") 'rotate-frame-clockwise)))

   ;; (:name fastnav
   ;;        :after (progn
   ;;                 (require 'fastnav)
   ;;                 (global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
   ;;                 (global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
   ;;                 (global-set-key "\M-s" 'fastnav-jump-to-char-forward)
   ;;                 (global-set-key "\M-S" 'fastnav-jump-to-char-backward)
   ;;                 (global-set-key "\M-r" 'fastnav-replace-char-forward)
   ;;                 (global-set-key "\M-R" 'fastnav-replace-char-backward)
   ;;                 (global-set-key "\M-i" 'fastnav-insert-at-char-forward)
   ;;                 (global-set-key "\M-I" 'fastnav-insert-at-char-backward)
   ;;                 (global-set-key "\M-j" 'fastnav-execute-at-char-forward)
   ;;                 (global-set-key "\M-J" 'fastnav-execute-at-char-backward)
   ;;                 (global-set-key "\M-k" 'fastnav-delete-char-forward)
   ;;                 (global-set-key "\M-K" 'fastnav-delete-char-backward)
   ;;                 (global-set-key "\M-m" 'fastnav-mark-to-char-forward)
   ;;                 (global-set-key "\M-M" 'fastnav-mark-to-char-backward)
   ;;                 (global-set-key "\M-p" 'fastnav-sprint-forward)
   ;;                 (global-set-key "\M-P" 'fastnav-sprint-backward)))

   ;; (:name thing-cmds
   ;;        :after (progn
   ;;                 (require 'thing-cmds)
   ;;                 (thgcmd-bind-keys)))

   ;; (:name expand-region
   ;;        :after (progn
   ;;                 (require 'expand-region)
   ;;                 (global-set-key (kbd "M-8") 'er/expand-region)))

   ;; Don't enable this. Breaks autocomplete and snippets.
   ;; ;; (:name js3-mode
   ;; ;;        :website "https://github.com/thomblake/js3-mode#readme"
   ;; ;;        :description "A chimeric fork of js2-mode and js-mode"
   ;; ;;        :type github
   ;; ;;        :pkgname "thomblake/js3-mode"
   ;; ;;        :pre-init (autoload 'js3-mode "js3" nil t))

   ;; (:name smart-forward
   ;;        :website "https://github.com/magnars/smart-forward.el#readme"
   ;;        :description "Semantic navigtation based on expand-region.el"
   ;;        :type github
   ;;        :depends (expand-region)
   ;;        :pkgname "magnars/smart-forward.el"
   ;;        :post-init (progn
   ;;                     (require 'smart-forward)
   ;;                     ;;(global-set-key (kbd "M-<up>") 'smart-up)
   ;;                     ;;(global-set-key (kbd "M-<down>") 'smart-down)
   ;;                     (global-set-key (kbd "M-<left>") 'smart-backward)
   ;;                     (global-set-key (kbd "M-<right>") 'smart-forward)
   ;;                     ))

   ;; (:name closure-template-html-mode
   ;;        :website "https://github.com/archimag/cl-closure-template#readme"
   ;;        :description "Google Closure Template mode."
   ;;        :type github
   ;;        :pkgname "archimag/cl-closure-template"
   ;;        :post-init (progn
   ;;                     (require 'closure-template-html-mode)
   ;;                     ))

   ;; (:name restclient
   ;;        :website "https://github.com/pashky/restclient.el#readme"
   ;;        :description "HTTP REST client for Emacs."
   ;;        :type github
   ;;        :pkgname "pashky/restclient.el"
   ;;        :post-init (progn
   ;;                     (require 'restclient)))

   (:name js2-refactor
          :website "https://github.com/magnars/js2-refactor.el#readme"
          :description "JavaScript refactoring library for Emacs."
          :type github
          :pkgname "magnars/js2-refactor.el"
          :post-init (progn
                       (require 'js2-refactor)))

   ;; (:name js-comint
   ;;        :after (progn
   ;;                 (require 'js-comint)
   ;;                 (setq inferior-js-program-command "node")
   ;;                 ;; (setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
   ;;                 (add-hook 'js-mode-hook '(lambda ()
   ;;                                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
   ;;                                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
   ;;                                            (local-set-key "\C-cb" 'js-send-buffer)
   ;;                                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
   ;;                                            (local-set-key "\C-cl" 'js-load-file-and-go)
   ;;                                            ))
   ;;                 ))

   ;; (:name protobuf-mode
   ;;        :after (progn
   ;;                 (require 'protobuf-mode)
   ;;                 (defconst g-protobuf-style
   ;;                   '((c-basic-offset . 2)
   ;;                     (indent-tabs-mode . nil)))
   ;;                 (add-hook 'protobuf-mode-hook
   ;;                           (lambda () (c-add-style "g-protobuf-style" g-protobuf-style t)))))

   ;; ;; (:name closure-lint-mode
   ;; ;;     :type github
   ;; ;;     :website "https://github.com/r0man/closure-lint-mode"
   ;; ;;     :description "Emacs support for the Closure Linter"
   ;; ;;     :pkgname "r0man/closure-lint-mode"
   ;; ;;     :after (progn
   ;; ;;              (require 'closure-lint-mode)))

   ;; ;; (:name closure-template-html-mode
   ;; ;;     :type github
   ;; ;;     :website "https://github.com/archimag/cl-closure-template"
   ;; ;;     :description "Emacs support for Google Closure Templates"
   ;; ;;     :pkgname "archimag/cl-closure-template"
   ;; ;;     :after (progn
   ;; ;;              (require 'closure-template-html-mode)))

   ;; (:name rst-mode
   ;;        :after (progn
   ;;                 (require 'rst)
   ;;                 (add-hook 'rst-adjust-hook 'rst-toc-update)
   ;;                 (setq auto-mode-alist
   ;;                       (append '(
   ;;                                 ("\\.txt$" . rst-mode)
   ;;                                 ("\\.rst$" . rst-mode)
   ;;                                 ("\\.rest$" . rst-mode))
   ;;                               auto-mode-alist))
   ;;                 ))

   ;; ;; (:name yaml-mode
   ;; ;;        :after (progn
   ;; ;;                 (require 'goog-config-yaml-mode)))

   ;; (:name ropemacs
   ;;        :after (progn
   ;;                 (require 'pymacs)
   ;;                 (autoload 'pymacs-apply "pymacs")
   ;;                 (autoload 'pymacs-call "pymacs")
   ;;                 (autoload 'pymacs-eval "pymacs" nil t)
   ;;                 (autoload 'pymacs-exec "pymacs" nil t)
   ;;                 (autoload 'pymacs-load "pymacs" nil t)
   ;;                 (pymacs-load "ropemacs" "rope-")
   ;;                 (setq ropemacs-enable-autoimport t)))
   ))

(setq
 goog:el-get-packages
 '(el-get
   ;; pymacs
   powerline
   ))
;; Synchronize el-get packages.
(setq goog:el-get-packages
      (append
       goog:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))
(el-get 'sync goog:el-get-packages)
;;(el-get 'wait)


;; Load our definitions.
(require 'goog-defuns)


;; ======================================================================
;; Vanilla Emacs preferences.
;; ======================================================================
(when window-system
  (blink-cursor-mode nil)
  (global-font-lock-mode 1)
  ;; (global-hl-line-mode)
  (mouse-wheel-mode t)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  )

(setq-default indent-tabs-mode nil
              indicate-empty-lines t
              indicate-buffer-boundaries (quote left))

(setq
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      color-theme-is-global t
      diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain
      inhibit-startup-message t
      mouse-yank-at-point t
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      read-file-name-completion-ignore-case 't
      save-place-file (concat user-emacs-directory "places")
      sentence-end-double-space nil
      shift-select-mode t            ;; Some of my users want this.
      uniquify-buffer-name-style 'forward
      visible-bell t
      ;; require-final-newline 't       ;; Don't require this for snippets.
      ;; next-line-add-newlines nil     ;; Don't add newlines past EOF.
      )

(progn
  (column-number-mode 1)             ;; show column numbers in mode line.
  (delete-selection-mode 1)          ;; Overwrite selection; DWIM.
  (fset 'yes-or-no-p 'y-or-n-p)      ;; y or n is better than yes or no.
  (global-auto-revert-mode 1)        ;; Auto-reflect on-disk changes.
  (global-linum-mode 1)              ;; line numbers in left gutter.
  (global-subword-mode t)            ;; Use subword navigation.
  (line-number-mode 1)               ;; show line numbers in the mode line.
  )

(progn
  (show-paren-mode t)                ;; Highlight matching parentheses.
  ;; (setq show-paren-style 'expression)
  )

;; Cua mode without the nonsense.
(setq cua-enable-cua-keys nil)       ;; Turn off Windows key bindings.
(cua-mode t)                         ;; Rectangular selections are awesome.
(cua-selection-mode nil)             ;; No shift-arrow style marking.

;; Saving sessions makes Emacs load really slowly. Enable this only if you
;; need it.
;; (progn
;;   (desktop-save-mode t)           ;; Save sessions.
;;   (desktop-load-default)          ;; Load the desktop on startup.
;;   (setq desktop-enable t))        ;; Automatically save desktop on exit.

;; ----------------------------------------------------------------------
;; Backups.
;; ----------------------------------------------------------------------
;; (setq backup-by-copying t             ;; don't clobber symlinks
;;       backup-directory-alist
;;       '(("." . "~/.emacs.d/saves"))   ;; don't litter my fs tree
;;       delete-old-versions t
;;       kept-new-versions 6
;;       kept-old-versions 2
;;       version-control t)              ;; use versioned backups
(setq make-backup-files nil)             ;; No backup files ~

;; ----------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------
(when window-system
  (when (goog/platform/is-darwin-p)
    ;; Monaco is clean. The default is too small in the GUI.
    (set-face-font 'default "Monaco-13")
    ;; Mac OS X-specific font anti-aliasing.
    (setq mac-allow-anti-aliasing t)))

;; Scroll faster.
(setq mouse-wheel-scroll-amount '(7 ((shift) . 1) ((control) . nil)))

;; Enables mouse support scrolling when using Emacs in the terminal.
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; ----------------------------------------------------------------------
;; Tabs.
;; ----------------------------------------------------------------------
;; (require 'tabbar)

;; ----------------------------------------------------------------------
;; Editing and searching.
;; ----------------------------------------------------------------------
(setq-default fill-column 80)     ;; right margin and fill column.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use whitespace mode. Cleans up trailing spaces, shows tabs, unnecessary
;; whitespace, end of file newline, bad indentation, text overrunning the
;; margin, etc.
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style
      '(face empty indentation lines-tail newline tabs trailing)
      whitespace-action '(auto-cleanup warn-read-only)
      whitespace-line-column 80)
;; Disabled space-mark newline-mark because it makes code very hard to read.
(setq whitespace-display-mappings
      '((space-mark   ?\    [?\xB7]     [?.])       ; space
        (space-mark   ?\xA0 [?\xA4]     [?_])       ; hard space
        (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])   ; end-of-line
        ))

;; Whitespace-aware kill-line.
(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "cleanup whitespace on kill-line"
  (if (not (bolp))
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;; Autofill mode.
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-common-hook
              (lambda ()
                (auto-fill-mode 1)
                (set (make-local-variable 'fill-nobreak-predicate)
                     (lambda ()
                       (not (eq (get-text-property (point) 'face)
                                'font-lock-comment-face))))))

(require 're-builder)
(setq reb-re-syntax 'string)

;; ----------------------------------------------------------------------
;; OS Clipboard.
;; ----------------------------------------------------------------------
(setq x-select-enable-clipboard t) ;; <3 clipboard copy-paste.
;; https://github.com/bbatsov/emacs-prelude/commit/d26924894b31d5dc3a8b2813719579baccc2b433
(when (goog/platform/is-darwin-p)
  (defun goog/clipboard/copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (defun goog/clipboard/paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'goog/clipboard/paste-to-osx
        interprogram-paste-function 'goog/clipboard/copy-from-osx))

;; ----------------------------------------------------------------------
;; File and directory navigation.
;; ----------------------------------------------------------------------
;; (require 'dired-x)     ;; C-x C-j jumps to current file in dired.

(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer
at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

;; (require 'ido)
;; (require 'ido-ubiquitous)
(eval-after-load "ido"
  '(progn
     (ido-mode t)
     (eval-after-load "ido-ubiquitous"
       '(progn
          (ido-ubiquitous t)
          ))
     (setq ido-save-directory-list-file (concat config-dir ".ido.last")
           ido-use-filename-at-point 'guess
           ido-enable-flex-matching t
           confirm-nonexistent-file-or-buffer nil
           ido-create-new-buffer 'always)

     ;; Display ido results vertically, rather than horizontally.
     ;; From the Emacs wiki.
     (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
     (defun ido-disable-line-trucation ()
       (set (make-local-variable 'truncate-lines) nil))
     (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
     ))

;; (global-set-key (kbd "M-i") 'ido-goto-symbol)

;; Recent files.
(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; Disable before we start recentf for tramp.
(recentf-mode t)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))


;; ======================================================================
;; goog/paredit
;; ======================================================================
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

;; Lispy languages
(setq goog/paredit/lisp-modes '(
                                emacs-lisp
                                lisp
                                lisp-interaction
                                scheme
                                clojure))

;; Non lispy languages.
(setq goog/paredit/non-lisp-modes '(
;;                                    js
;;                                    js3
;;                                    js2
;;                                    javascript
;;                                    python
                                    java
                                    c))

(defun goog/paredit/singlequote (&optional n)
  "Insert a pair of single-quotes.
With a prefix argument N, wrap the following N S-expressions in
  single-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of single-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal single-quote.
At the end of a string, move past the closing single-quote.
In the middle of a string, insert a backslash-escaped single-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)             ; We're on the closing quote.
             (insert ?\\ ?\' )))
        ((paredit-in-comment-p)
         (insert ?\' ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\' ?\' 'paredit-forward-for-quote))))

(defun goog/paredit/lisp ()
  "Enables paredit mode for lisp."
  (paredit-mode +1))

(defun goog/paredit/non-lisp ()
  "Enables paredit mode for non-lisp languages. Does not insert
spaces before opening parentheses.  See: https://gist.github.com/879305"
  (add-to-list (make-local-variable
                'paredit-space-for-delimiter-predicates)
               (lambda (_ _) nil))
  (enable-paredit-mode))


;; Enable paredit-mode for these major modes.
(dolist (mode goog/paredit/lisp-modes)
  (add-hook (intern (format "%s-mode-hook" mode)) 'goog/paredit/lisp))

(dolist (mode goog/paredit/non-lisp-modes)
  (add-hook (intern (format "%s-mode-hook" mode)) 'goog/paredit/non-lisp))


(require 'eldoc) ; if not already loaded
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; (defun maybe-map-paredit-newline ()
;;   (unless (or (eq major-mode 'inferior-emacs-lisp-mode) (minibufferp))
;;     (local-set-key (kbd "RET") 'paredit-newline)))

;; (add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(eval-after-load 'paredit
  '(progn
     ;; These are handy everywhere, not just in lisp modes
     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)

     (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                            (kbd "C-M-<left>") (kbd "C-M-<right>")))
       (define-key paredit-mode-map binding nil))

     ;; Disable kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map [remap kill-sentence] nil)
     (define-key paredit-mode-map [remap backward-kill-sentence] nil)))

;; Compatibility with other modes
(defadvice enable-paredit-mode (before disable-autopair activate)
  (setq autopair-dont-activate t)
  (autopair-mode -1))
;; (suspend-mode-during-cua-rect-selection 'paredit-mode)

;; Use paredit in the minibuffer
(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc)
  "Interactive commands for which paredit should be enabled in the
minibuffer.")


;; ----------------------------------------------------------------------
;; goog/edit
;; ----------------------------------------------------------------------

(defun goog/edit/set-newline-and-indent ()
  "Sets RET to do a newline and indent."
  (local-set-key (kbd "RET") 'newline-and-indent))


;; Alphabet case

(defun goog/edit/watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|NOTE\\|WARNING\\(S\\)?\\|CAUTION\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; ----------------------------------------------------------------------
;; goog/elisp
;; ----------------------------------------------------------------------
(defun goog/elisp/load-current-module ()
  "Loads the current Elisp module."
  (interactive)
  (load-file buffer-file-name))

(defun goog/elisp/load-directory (dir)
  "Loads all elisp modules from a given directory."
  (when (file-exists-p dir)
    (mapc 'load (directory-files dir t "^[^#].*el"))))

(defun goog/elisp/load-if-exists (name)
  "Loads an elisp module if it exists."
  (when (file-exists-p name)
    (load name)))

(defun goog/elisp/reload-user-init-file ()
  "Reloads the init.el module from the Emacs configuration directory."
  (interactive)
  (load-file (concat config-dir "init.el")))

(defun goog/elisp/eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM
immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; ======================================================================
;; Packages
;; ======================================================================

;; (require 'flex-isearch)
;; (global-flex-isearch-mode)

;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;; Let the power to type multiple keys at the same time be yours.
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; Prevents: http://code.google.com/p/autopair/issues/detail?id=32
(add-hook 'sldb-mode-hook
          #'(lambda ()
              (setq autopair-dont-activate t) ;; emacs < 24
              (autopair-mode -1)              ;; emacs >= 24
              ))
(set-default 'autopair-dont-activate
             #'(lambda ()
                 (eq major-mode 'sldb-mode)))

(require 'undo-tree)

(require 'nav)
(global-set-key (kbd "C-x C-a") 'nav)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'move-text)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

(require 'switch-window)

;; Need to test this properly.
;; (when window-system
;;   (require 'fill-column-indicator)
;;   (define-globalized-minor-mode global-fci-mode fci-mode
;;     (lambda () (fci-mode 1)))
;;   (global-fci-mode 1))

(require 'expand-region)
(global-set-key (kbd "M-8") 'er/expand-region)

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

;; Find things fast.
(require 'find-things-fast)
(global-set-key (kbd "C-x f") 'ftf-find-file)
(global-set-key (kbd "<f6>") 'ftf-grepsource)

;; ----------------------------------------------------------------------
;; Mark and edit multiple regions.
;; ----------------------------------------------------------------------
(require 'iedit)
(put 'narrow-to-region 'disabled nil)  ;; Allow narrowing to work.

(require 'mark-multiple)
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(require 'sgml-mode)
(require 'rename-sgml-tag)
(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; JS2 refactor stuff is defined in el-get load.

(require 'perspective)
(persp-mode)

(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-F") 'helm-for-files)

;; ======================================================================
;; Auto-complete and snippets.
;; ======================================================================
(require 'dabbrev)

(require 'yasnippet)
(setq yas/snippet-dirs
      '("~/.emacs.d/snippets"  ;; Our snippets.
        ;; Add more paths here if you like.
        ))
(yas/global-mode 1)  ;; or M-x yas/reload-all to reload yasnippet.


(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case 'smart
      ;; ac-use-fuzzy t
      ;; ac-fuzz-enable t
      ;; ac-auto-start t
      ac-dwim t
      )
;; Don't use tab to cycle. It's irritating.
(define-key ac-completing-map "\t" 'ac-complete)
;; (define-key ac-completing-map "\r" nil)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
(setq tab-always-indent 'complete)  ;; use 'complete when auto-complete
                                    ;; is disabled
(add-to-list 'completion-styles 'initials t)
;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)

;; Set up sources for autocompletion.
(setq-default ac-sources
              '(ac-source-abbrev
                ac-source-dictionary
                ac-source-filename
                ac-source-files-in-current-dir
                ac-source-imenu
                ac-source-words-in-all-buffer
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-yasnippet
                ))
(dolist (mode '(
                clojure-mode
                css-mode
                csv-mode
                espresso-mode
                haml-mode
                haskell-mode
                html-mode
                js3-mode
                less-css-mode
                lisp-mode
                log-edit-mode
                magit-log-edit-mode
                markdown-mode
                nxml-mode
                org-mode
                sass-mode
                sh-mode
                smarty-mode
                text-mode
                textile-mode
                tuareg-mode
                yaml-mode
                ))
  (add-to-list 'ac-modes mode))

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)


;; ======================================================================
;; Programming-specific.
;; ======================================================================

;; Automatically set executable permissions on executable script files.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Highlight watchwords.
(add-hook 'prog-mode-hook 'goog/edit/watchwords)

;; ----------------------------------------------------------------------
;; sh-mode
;; ----------------------------------------------------------------------
(defun goog/config/sh-mode/setup-style ()
  "Sets up the style for sh-mode."
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook 'goog/config/sh-mode/setup-style)

;; ----------------------------------------------------------------------
;; html-mode
;; ----------------------------------------------------------------------
;; Don't use tabs when indenting in HTML mode.
(add-hook
 'html-mode-hook
 '(lambda ()
    (set (make-local-variable 'sgml-basic-offset) 2)
    (setq indent-tabs-mode nil)
    (zencoding-mode 1)))

;; ----------------------------------------------------------------------
;; JavaScript
;; ----------------------------------------------------------------------
(autoload 'js2-mode "js2-mode" nil t)
(eval-after-load "js2-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
     (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
     (add-hook 'js2-mode-hook 'goog/config/js-mode/key-chords)
     (add-hook 'js2-mode-hook 'goog/config/js-mode/setup-bindings)
     (add-hook 'js2-mode-hook 'goog/config/js-mode/setup-style)))

(defun goog/config/js-mode/setup-style ()
  "Sets the style for JavaScript."
  (setq js-indent-level 2
        espresso-indent-level 2
        c-basic-offset 2
        js2-basic-offset 2
        js2-indent-on-enter-key nil
        js2-enter-indents-newline t
        js2-mode-squeeze-spaces nil
        ;; js2-bounce-indent-p t
        js2-auto-indent-p t))

(defun goog/config/js-mode/key-chords ()
  "Sets up the key-chords for JavaScript mode."
  (key-chord-define js2-mode-map ";;"  "\C-e;")
  (key-chord-define js2-mode-map ",,"  "\C-e,")
  )

(add-hook 'js-mode-hook 'goog/config/js-mode/setup-style)
(add-hook 'js-mode-hook 'goog/config/js-mode/key-chords)

;; Tools for Javascript.
(defun goog/config/js-mode/gjslint-buffer ()
  "Runs gjslint in strict mode on the current buffer."
  (interactive)
  (compile (concat "gjslint --strict --unix_mode " buffer-file-name)))

(defun goog/config/js-mode/gjslint-dir ()
  "Runs gjslint in strict mode on the parent directory of the file in the
current buffer."
  (interactive)
  (compile (concat "gjslint --strict --unix_mode -r "
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

(defun goog/config/js-mode/setup-bindings ()
  "Sets up keyboard bindings for JavaScript modes."
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
    'goog/config/js-mode/jshint))

;; ----------------------------------------------------------------------
;; Python.
;; ----------------------------------------------------------------------
(defun goog/config/python-mode/setup-style ()
  "Defines the python coding style."
  (setq indent-tabs-mode nil
        require-final-newline 't
        tab-width 2
        python-indent-offset 2
        python-indent 2
        py-indent-offset 2))
(setq auto-mode-alist
      (append '(
                ("\\wscript$" . python-mode)
                ("\\SConstruct" . python-mode)
                ("\\SConscript" . python-mode)
                ("\\BUILD$" . python-mode))
              auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
          'goog/config/python-mode/setup-style)


;; ======================================================================
;; Keyboard bindings.
;; ======================================================================
;; Shift region left/right.
(global-set-key (kbd "s-]") 'shift-right)
(global-set-key (kbd "s-[") 'shift-left)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Increase/decrease/reset font size.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") '(lambda ()
                               (interactive)
                               (text-scale-set 0)
                               ))

;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-s") 'flex-isearch-forward)
;; (global-set-key (kbd "\C-r") 'flex-isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Killing and yanking.
(define-key global-map (kbd "<delete>") 'delete-char)
(define-key global-map (kbd "M-<delete>") 'kill-word)
;; (global-set-key (kbd "C-k") 'kill-whole-line)
;; (global-set-key (kbd "C-S-<backspace>")
;;                 'kill-and-join-forward)

;; Line insertion
(global-set-key (kbd "S-<return>") 'insert-blank-line-below)
(global-set-key (kbd "M-S-<return>") 'insert-blank-line-above)
(global-set-key (kbd "s-<return>")
                'insert-blank-line-below-next-line)

;; Line or region duplication.
(global-set-key (kbd "C-c C-d") 'duplicate-current-line-or-region)

;; Toggle identifier case.
(global-set-key (kbd "C-x t c")
                'toggle-identifier-naming-style)

;; Sort lines
(global-set-key (kbd "C-c s") 'sort-lines)

;; Jump easily between beginning and end of defuns.
(global-set-key (kbd "s-<up>") 'beginning-of-defun)
(global-set-key (kbd "s-<down>") 'end-of-defun)

;; Parentheses matching.
(global-set-key (kbd "M-0") 'goto-match-paren)

;; Open recently opened files quickly.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; Occur.
;; (global-set-key (kbd "<f6>") 'occur)
(global-set-key (kbd "<f7>") 'multi-occur-in-this-mode)

;; Quotes.
(global-set-key (kbd "C-'") 'toggle-quotes)

;; Transpose parameters.
(global-set-key (kbd "s-t") 'transpose-params)

;; Perspectives.
(global-set-key (kbd "s-<left>") 'persp-prev)
(global-set-key (kbd "s-<right>") 'persp-next)

;; ----------------------------------------------------------------------
;; Key chords
;; ----------------------------------------------------------------------
(key-chord-define-global ",."     "<>\C-b")
(key-chord-define-global "hj" 'undo)
;;(key-chord-define-global [?h ?j]  'undo)  ; the same
(key-chord-define-global "jk" 'dabbrev-expand)
(key-chord-define-global "zx" "\C-a\C-k")
;; (key-chord-define-global "pp" 'yank)
;; (key-chord-define-global "xx" 'kill-region)
;; (key-chord-define-global "cc" 'kill-ring-save)

;;(key-chord-define-global "cv"     'reindent-then-newline-and-indent)
;;(key-chord-define-global "4r"     "$")


;; ----------------------------------------------------------------------
;; Now load host, os, network-specific configuration.
;; ----------------------------------------------------------------------
(progn
  ;; ~/.emacs.d/*
  (setq goog-local-hosts-dir (concat config-dir "host/"))
  (setq goog-local-users-dir (concat config-dir "user/"))
  (setq goog-local-os-dir (concat config-dir "os/"))

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
  (setq goog-os-dir (concat goog-local-os-dir (format "%s" system-type)))

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
      )))

;;; init.el ends here.
