;;; gemacs-appearance.el --- Appearance related stuff.
;;
;;; Commentary:
;;
;;; Code:

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

;; Additional faces.
(defvar font-lock-operator-face 'font-lock-operator-face)
(defface font-lock-operator-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "dark red"))
    (t nil))
  "Used for operators."
  :group 'font-lock-faces)
(defvar font-lock-end-statement-face 'font-lock-end-statement-face)
(defface font-lock-end-statement-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "DarkSlateBlue"))
    (t nil))
  "Used for end statement symbols."
  :group 'font-lock-faces)
(defvar font-lock-operator-keywords
  '(("\\([][|!.+=&/%*,<>(){}:^~-]+\\)" 1 font-lock-operator-face)
    (";" 0 font-lock-end-statement-face)))


;; Now set the default theme.
(require 'golokai-theme)
(powerline-default-theme)

(provide 'gemacs-appearance)

;;; gemacs-appearance.el ends here
