;; Copyright 2012 Google Inc. All Rights Reserved.
;;
;; Author: yesudeep@google.com (Yesudeep Mangalapilly)
;;
;; Description:
;;   All of the configuration has been placed into a single file
;;   to load it extremely quickly even when using NFS.


(require 'cl)

;; ======================================================================
;; Initial configuration.
;; ======================================================================
(setq config-dir (file-name-directory (or (buffer-file-name)
                                          load-file-name)))
(add-to-list 'load-path config-dir)

;; Directory paths.
(setq third-party-dir (expand-file-name (concat config-dir "third_party")))

;; Load-path.
(add-to-list 'load-path third-party-dir)

;; Add all subdirectories of third_party/ to load path.
(let ((default-directory third-party-dir))
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
                           find-things-fast
                           ;; flex-isearch
                           ;; icicles
                           ;; duplicate-thing
                           autopair
                           expand-region
                           fastnav
                           fill-column-indicator
                           ido-ubiquitous
                           iedit
                           magit
                           move-text
                           paredit
                           switch-window
                           undo-tree
                           key-chord
                           powerline
                           ))
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ======================================================================
;; Vanilla Emacs preferences.
;; ======================================================================
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
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      visible-bell t
      require-final-newline 't
      next-line-add-newlines t
      )

(progn
  (column-number-mode 1)             ;; show column numbers in mode line.
  (delete-selection-mode t)          ;; Overwrite selection; DWIM.
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

;; Enables mouse scrolling when using Emacs in the terminal.
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1))))

;; ----------------------------------------------------------------------
;; Editing
;; ----------------------------------------------------------------------
(setq-default fill-column 75)     ;; right margin and fill column.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Use whitespace mode. Cleans up trailing spaces, shows tabs, unnecessary
;; whitespace, end of file newline, bad indentation, text overrunning the
;; margin, etc.
(require 'whitespace)
(global-whitespace-mode t)
(setq whitespace-style
      '(face empty indentation lines-tail newline tabs trailing)
      whitespace-action '(auto-cleanup warn-read-only)
      whitespace-line-column 75)

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

;; Recent files.
(require 'recentf)
;; Disable before we start recentf for tramp.
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)


;; ======================================================================
;; goog/ido
;; ======================================================================
(require 'ido)
(require 'ido-ubiquitous)

(defun goog/config/ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (ido-find-file-in-dir default-directory))))

(ido-mode t)
(ido-ubiquitous t)
(setq ido-save-directory-list-file (concat config-dir ".ido.last")
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t)


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
  (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
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
  "Interactive commands for which paredit should be enabled in the minibuffer.")


;; ----------------------------------------------------------------------
;; goog/edit
;; ----------------------------------------------------------------------
;; @see http://tuxicity.se/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun goog/edit/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun goog/edit/shift-region (distance)
  "Shift the selected region right if distance is positive; left if negative."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun goog/edit/shift-right ()
  "Shifts the selection toward the right."
  (interactive)
  (goog/edit/shift-region 1))

(defun goog/edit/shift-left ()
  "Shifts the selection toward the left."
  (interactive)
  (goog/edit/shift-region -1))

(defun goog/edit/untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun goog/edit/tabify-buffer ()
  "Tabify the entire buffer."
  (interactive)
  (tabify (point-min) (point-max)))

(defun goog/edit/indent-buffer ()
  "Properly indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun goog/edit/tidy-buffer ()
  "Indent, untabify, and clean up trailing whitespace from a buffer."
  (interactive)
  (goog/edit/indent-buffer)
  (goog/edituntabify-buffer)
  (delete-trailing-whitespace))

;; Line insertion
(defun goog/edit/insert-blank-line-below ()
  "Inserts blank line below current line."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun goog/edit/insert-blank-line-above ()
  "Inserts blank line above current line."
  (interactive)
  (previous-line 1)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun goog/edit/insert-blank-line-below-next-line ()
  "Inserts an empty line below the next line."
  (interactive)
  (next-line 1)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun goog/edit/set-newline-and-indent ()
  "Sets RET to do a newline and indent."
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Kill entire line with C-k and use C-S-backspace for killing from beginning
(defun goog/edit/kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
    Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

;; From: https://www.bunkus.org/blog/2009/12/switching-identifier-naming-style-between-camel-case-and-c-style-in-emacs/
(defun goog/edit/toggle-identifier-naming-style ()
  "Toggles the symbol at point between C-style naming,
e.g. `hello_world_string', and camel case,
e.g. `HelloWorldString'."
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search symbol-at-point cstyle regexp func)
    (unless symbol-pos
      (error "No symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))

;; Alphabet case
(defun goog/edit/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: Òall lowerÓ, ÒInit CapsÓ, ÒALL CAPSÓ."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ))

(defun goog/edit/camelcase-to-upcased-underscore-string (s &optional sep start)
  "Convert CamelCase string S to upper case with word separator SEP.
Default for SEP is a underscore \"_\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                             (downcase (match-string 0 s)))
                                     t nil s)))
    (upcase s)))

(defun goog/edit/camelcase-to-downcased-underscore-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a underscore \"_\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "_")
                                             (downcase (match-string 0 s)))
                                     t nil s)))
    (downcase s)))

(defun goog/edit/camelcase-to-downcased-hyphenated-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphenated \"-\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                             (downcase (match-string 0 s)))
                                     t nil s)))
    (downcase s)))

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
  "thisandthat."
  (interactive)
  (load-file (concat config-dir "init.el")))

(defun goog/elisp/eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

;; ======================================================================
;; Packages
;; ======================================================================
;; (require 'flex-isearch)

;; (require 'icicles)
;; (icy-mode 1)

;; (require 'duplicate-thing)
;; (global-set-key (kbd "M-c") 'duplicate-thing)

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

;; (require 'nav)
;; (global-set-key (kbd "C-x C-a") 'nav)

(require 'iedit)
(put 'narrow-to-region 'disabled nil)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'move-text)
(global-set-key [M-S-up] 'move-text-up)
(global-set-key [M-S-down] 'move-text-down)

(when window-system
  (require 'fill-column-indicator)
  (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode 1))

(require 'powerline)

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
    (setq indent-tabs-mode nil)))


;; ======================================================================
;; Keyboard bindings.
;; ======================================================================
;; Shift region left/right.
(global-set-key (kbd "M-]") 'goog/edit/shift-right)
(global-set-key (kbd "M-[") 'goog/edit/shift-left)
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
;;                 'goog/edit/kill-and-join-forward)

;; Line insertion
(global-set-key (kbd "S-<return>") 'goog/edit/insert-blank-line-below)
(global-set-key (kbd "M-S-<return>") 'goog/edit/insert-blank-line-above)
(global-set-key (kbd "s-<return>")
                'goog/edit/insert-blank-line-below-next-line)

;; Line or region duplication.
(global-set-key (kbd "C-c C-d")
                'goog/edit/duplicate-current-line-or-region)

;; Toggle identifier case.
(global-set-key (kbd "C-x t c")
                'goog/edit/toggle-identifier-naming-style)

;; Refill mode.
(global-set-key (kbd "C-c q") 'refill-mode)

;; Sort lines
(global-set-key (kbd "C-c s") 'sort-lines)

;; ----------------------------------------------------------------------
;; Key chords
;; ----------------------------------------------------------------------
(key-chord-define-global ",."     "<>\C-b")
(key-chord-define-global "hj"     'undo)
;;(key-chord-define-global [?h ?j]  'undo)  ; the same
(key-chord-define-global "jk"     'dabbrev-expand)
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
