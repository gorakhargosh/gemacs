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


;; ======================================================================
;; Package management.
;; ======================================================================

;; ----------------------------------------------------------------------
;; El-get
;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(setq
 el-get-sources
 '(el-get
   (:name smex              ;; a better (ido-like) M-x
          :after (progn
                   (setq smex-save-file "~/.emacs.d/.smex-items")
                   (global-set-key (kbd "M-x") 'smex)
                   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name go-mode
          :website "http://github.com/dominikh/go-mode.el#readme"
          :description "An improved go-mode."
          :type github
          :pkgname "dominikh/go-mode.el"
          :post-init (progn
                       (require 'go-mode)))
   ))
(setq
 goog:el-get-packages
 '(
   auto-complete
   diff-hl
   expand-region
   fill-column-indicator
   js2-mode
   js2-refactor
   magit
   ))
(setq goog:el-get-packages
      (append
       goog:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

(el-get 'sync goog:el-get-packages)

;; ----------------------------------------------------------------------
;; package.el
;; ----------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar default-packages '(
                           ace-jump-mode
                           ack-and-a-half
                           auto-compile
                           dart-mode
                           evil-numbers
                           exec-path-from-shell
                           fastnav
                           find-things-fast
                           flycheck
                           go-autocomplete
                           go-eldoc
                           helm
                           highlight-symbol
                           ido-ubiquitous
                           iedit
                           key-chord
                           less-css-mode
                           markdown-mode
                           multiple-cursors
                           nav
                           persp-mode
                           powerline
                           protobuf-mode
                           rainbow-delimiters
                           rcirc-color
                           sass-mode
                           smartparens
                           sql-indent
                           switch-window
                           tup-mode
                           undo-tree
                           vimrc-mode
                           yaml-mode
                           yasnippet
                           zencoding-mode
                           ))
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;------------------------------------------------------------------------------
;; End package management.
;;------------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Copy environment variables on Mac OS X because Apple is an asshole
;; and breaks everything all the time.
;; -----------------------------------------------------------------------------
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Autocompilation.
(add-to-list 'load-path user-emacs-directory)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

;; Load our definitions.
(require 'goog-defuns)

;; ======================================================================
;; Vanilla Emacs preferences.
;; ======================================================================

;; Maximize the Emacs frame on startup.
;; TODO(yesudeep): resolve a bug where it hides the first line of every buffer.
;;(require 'maxframe)
;;(add-hook 'window-setup-hook 'maximize-frame t)

(when window-system
  (blink-cursor-mode nil)
  (global-font-lock-mode 1)
  (global-hl-line-mode)
  (mouse-wheel-mode t)
  (scroll-bar-mode -1)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1)
  (tooltip-mode -1)
  )

(setq-default indent-tabs-mode nil
              indicate-empty-lines t
              indicate-buffer-boundaries (quote left)
              require-final-newline t
              cursor-type 'bar
              ;; next-line-add-newlines nil     ;; Don't add newlines past EOF.
              )

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
      visible-bell t
      )


(add-hook 'snippet-mode-hook (lambda ()
                               (setq require-final-newline nil)
                               ;; Don't enable this for snippets.
                               ))

(progn
  (column-number-mode 1)             ;; show column numbers in mode line.
  (delete-selection-mode 1)          ;; Overwrite selection; DWIM.
  (fset 'yes-or-no-p 'y-or-n-p)      ;; y or n is better than yes or no.
  (global-auto-revert-mode 1)        ;; Auto-reflect on-disk changes.
  (global-linum-mode 1)              ;; line numbers in left gutter.
  (global-subword-mode t)            ;; Use subword navigation.
  (line-number-mode 1)               ;; show line numbers in the mode line.
  )

;; Cua mode without the nonsense.
;; (setq cua-enable-cua-keys nil)       ;; Turn off Windows key bindings.
;; (cua-mode t)                         ;; Rectangular selections are awesome.
;; (cua-selection-mode nil)             ;; No shift-arrow style marking.
(delete-selection-mode t)

;; Use unique buffer names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

(setq backup-directory-alist `((".*" . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves" t)))

;; ----------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------

;; Fonts appear to be larger on Linux workstations.
(setq preferred-linux-fonts
      '(
        ;; Monaco is available on Mac OS X and some of my Linux workstations.
        "Monaco-13"

        ;; Ubuntu Linux has this.
        "Ubuntu Mono-12"

        ;; Used by the secure shell app on Chrome. Looks pretty.
        "DejaVu Sans Mono-12"

        ;; Droid Sans Mono: quite nice. 15 pixels total height at 10 point.
        ;; Clear & crisp. (e.g.
        ;; http://www.fontex.org/download/Droid-sans-mono.ttf)
        "Droid Sans Mono Dotted-10"
        "Droid Sans Mono-10"

        ;; Consolas: download installer from Microsoft.
        ;; Quite beautiful and renders nicely, but a little light.
        ;; Pretty similar to Droid Sans Mono.
        ;; The slanted verticals on the capital M annoy me a little.
        ;; (16 pixels height)
        "Consolas-10.5"

        ;; Inconsolata: lots of people like this.
        ;; http://www.levien.com/type/myfonts/inconsolata.html: about same size
        ;; as Consolas-10.5, but thicker and less leading (17 pixels height) and
        ;; not as smooth lines. Feels chunky.
        "Inconsolata-12"

        ;; default
        "Courier New-10.5"))

;; Mac OS X tends to show smaller fonts.
(setq preferred-mac-fonts
      '(
        ;; Monaco is available on Mac OS X.
        "Monaco-13"

        ;; Used by the secure shell app on Chrome. Looks pretty.
        "DejaVu Sans Mono-14"

        ;; Droid Sans Mono: quite nice. 15 pixels total height at 10 point.
        ;; Clear & crisp. (e.g.
        ;; http://www.fontex.org/download/Droid-sans-mono.ttf)
        "Droid Sans Mono Dotted-14"
        "Droid Sans Mono-14"

        ;; Ubuntu Linux has this but my Mac also does.
        "Ubuntu Mono-18"

        ;; Consolas: download installer from Microsoft. Quite beautiful and
        ;; renders nicely, but a little light. Pretty similar to Droid Sans
        ;; Mono. The slanted verticals on the capital M annoy me a little. (16
        ;; pixels height)
        "Consolas-10.5"

        ;; Inconsolata: lots of people like this.
        ;; http://www.levien.com/type/myfonts/inconsolata.html: about same size
        ;; as Consolas-10.5, but thicker and less leading (17 pixels height) and
        ;; not as smooth lines. Feels chunky.
        "Inconsolata-12"

        ;; default
        "Courier New-10.5"))

(defun find-first-font (list)
  (cond ((null list) nil)
        ((x-list-fonts (car list))
         (message (concat "Using font " (car list)))
         (car list))
        (t                              ; recurse
         (find-first-font (cdr list)))
        ))

(when window-system
  ;; set default font attributes (for all frames)
  (when (goog/platform/is-darwin-p)
    ;; Monaco is clean. The default is too small in the GUI.
    ;; (set-face-font 'default "Monaco-13")
    ;; Mac OS X-specific font anti-aliasing.
    (set-face-attribute 'default nil :font (find-first-font preferred-mac-fonts))
    (setq mac-allow-anti-aliasing t))
  (when (goog/platform/is-linux-p)
    ;; Monaco is clean. The default is too small in the GUI.
    ;; (set-face-font 'default "Monaco-13")
    ;; Mac OS X-specific font anti-aliasing.
    (set-face-attribute 'default nil :font (find-first-font preferred-linux-fonts)))
  )

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
  ;; (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; ----------------------------------------------------------------------
;; Editing and searching.
;; ----------------------------------------------------------------------
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
      '((space-mark   ?\    [?\xB7]     [?.])       ;; space
        (space-mark   ?\xA0 [?\xA4]     [?_])       ;; hard space
        (newline-mark ?\n   [?\xB6 ?\n] [?$ ?\n])   ;; end-of-line
        ))

(require 're-builder)
(setq reb-re-syntax 'string)

;; -----------------------------------------------------------------------------
;; Configure pairs of characters and parenthese matching.
;;
(require 'smartparens)
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(progn
  (show-paren-mode t)                ;; Highlight matching parentheses.
  ;; (setq show-paren-delay 0
  ;;       show-paren-style 'expression
  ;;       )
  (smartparens-global-mode t)
  (show-smartparens-global-mode +1)

  ;; Don't automatically escape quotes within quotes.
  (setq sp-autoescape-string-quote nil)
  )

;; (require 'rainbow-delimiters)
;; (global-rainbow-delimiters-mode)

;; Enable this if you need it.
;; (add-hook 'prog-mode-hook '(lambda () (rainbow-mode)))

;; ----------------------------------------------------------------------
;; Clipboard and kill-ring.
;; ----------------------------------------------------------------------
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

(require 'eldoc) ; if not already loaded
(require 'switch-window)

(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(require 'persp-mode)
(persp-mode t)

(require 'helm-config)
(helm-mode 1)

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

;; ======================================================================
;; Packages
;; ======================================================================

;; Git integration.
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(eval-after-load "magit"
  '(progn
     (setq magit-completing-read-function 'magit-ido-completing-read)
     ))


(require 'undo-tree)

(autoload 'nav "nav" nil t)
(global-set-key (kbd "C-x C-a") 'nav)

(autoload 'move-text-up "move-text" nil t)
(autoload 'move-text-down "move-text" nil t)
(global-set-key [M-s-up] 'move-text-up)
(global-set-key [M-s-down] 'move-text-down)

;; Need to test this properly.
;; (when window-system
;;   (require 'fill-column-indicator)
;;   (add-hook 'after-change-major-mode-hook 'fci-mode)
;;   (define-globalized-minor-mode global-fci-mode fci-mode
;;     (lambda () (fci-mode 1)))
;;   (global-fci-mode 1)
;;   (setq fci-rule-color "red")
;;   )

(autoload 'expand-region "expand-region" nil t)
(autoload 'contract-region "expand-region" nil t)
(global-set-key (kbd "M-8") 'er/expand-region)
(global-set-key (kbd "M-7") 'er/contract-region)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-.") 'ace-jump-mode)

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
(defvar ftf-filetypes
  '("*")
  "A list of filetype patterns that grepsource will use.")
(autoload 'ftf-find-file "find-things-fast" nil t)
(autoload 'ftf-grepsource "find-things-fast" nil t)
(global-set-key (kbd "C-x f") 'ftf-find-file)
(global-set-key (kbd "<f6>") 'ftf-grepsource)

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

;; ----------------------------------------------------------------------
;; Mark and edit multiple regions.
;; ----------------------------------------------------------------------
(require 'iedit)
(put 'narrow-to-region 'disabled nil)  ;; Allow narrowing to work.

(require 'multiple-cursors)
;; From active region to multiple cursors:
(global-set-key (kbd "C-, C-l") 'mc/edit-lines)
(global-set-key (kbd "C-, C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-, C-a") 'mc/edit-beginnings-of-lines)

;; Mark more like this.
(global-set-key (kbd "C-, C-;") 'mc/mark-all-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-, C-h") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-, C-d") 'mc/mark-all-symbols-like-this-in-defun)
(global-set-key (kbd "C-, C-,") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-, C-r") 'mc/mark-all-in-region)

(require 'sgml-mode)
(autoload 'rename-sgml-tag "rename-sgml-tag" nil t)
(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; ======================================================================
;; Auto-complete and snippets.
;; ======================================================================
(require 'dabbrev)
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"  ;; Our snippets.
        ;; Add more paths here if you like.
        ))
(yas-global-mode 1)  ;; or M-x yas/reload-all to reload yasnippet.

(require 'auto-complete)
(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)
(setq ac-ignore-case t
      ac-use-fuzzy t
      ac-auto-start 0
      ac-auto-show-menu 0.2
      ac-expand-on-auto-complete nil
      ac-dwim t)

;; Use TAB to complete, not cycle.
(define-key ac-completing-map "\t" 'ac-complete)
;; Disable RET completion.
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map [return] nil)

(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "C-n") 'ac-next)
(define-key ac-menu-map (kbd "C-p") 'ac-previous)

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; Set up sources for autocompletion.
(setq-default ac-sources
              '(ac-source-abbrev
                ac-source-dictionary
                ac-source-filename
                ac-source-files-in-current-dir
                ac-source-imenu
                ;; ac-source-words-in-all-buffer
                ac-source-words-in-buffer
                ac-source-words-in-same-mode-buffers
                ac-source-yasnippet
                ))

;; (global-auto-complete-mode t)
;; Dirty fix to enable AC everywhere without bothering about the ac-modes list.
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)


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



;; Appearance.
(require 'gemacs-appearance)

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
(require 'gemacs-sh)
(require 'gemacs-sql)
(require 'gemacs-tup)

;; Tools.
(require 'gemacs-flycheck)

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
