;;; golokai-theme.el --- Yet another golokai theme for Emacs 24

;; Copyright (C) 2013 Huang Bin
;; Copyright (C) 2014 Google Inc.

;; Author: Huang Bin <embrace.hbin@gmail.com>
;; URL: https://github.com/hbin/golokai-theme
;; Version: 0.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is another golokai dark theme for Emacs 24.
;; Equiped with my favorites.

;;; Requirements:
;;
;; Emacs 24

;;; Code:
(deftheme golokai "The golokai color theme for Emacs 24")

(let ((class '((class color) (min-colors 89)))
      ;; golokai palette
      (golokai-white          "#ffffff")
      (golokai-fg             "#f8f8f0")
      (golokai-red            "#ff0000")
      (golokai-pink           "#f92672")
      (golokai-orange+5       "#ef5939")
      (golokai-orange         "#fd971f")
      (golokai-yellow         "#ffff00")
      (golokai-darkgoldenrod  "#e6db74")
      (golokai-wheat          "#c4be89")
      (golokai-olive          "#808000")
      (golokai-chartreuse     "#a6e22e")
      (golokai-lime           "#00ff00")
      (golokai-green          "#008000")
      (golokai-darkwine       "#1e0010")
      (golokai-maroon         "#800000")
      (golokai-wine           "#960050")
      (golokai-teal           "#008080")
      (golokai-aqua           "#00ffff")
      (golokai-blue           "#66d9ef")
      (golokai-slateblue      "#7070f0")
      (golokai-purple         "#ae81ff")
      (golokai-palevioletred  "#d33682")
      (golokai-grey-2         "#bcbcbc")
      (golokai-grey-1         "#8f8f8f")
      (golokai-grey           "#808080")
      (golokai-grey+2         "#403d3d")
      (golokai-grey+3         "#4c4745")
      (golokai-grey+5         "#232526")
      ;; (golokai-bg             "#1b1d1e")
      (golokai-bg             "#000000")
      (golokai-grey+10        "#080808")
      (golokai-dark           "#000000")
      (golokai-base01         "#465457")
      (golokai-base02         "#455354")
      (golokai-base03         "#293739")
      (golokai-dodgerblue     "#13354a"))
  (custom-theme-set-faces
   'golokai

   ;; base
   `(default ((t (:background ,golokai-bg :foreground ,golokai-fg))))
   `(cursor ((t (:background ,golokai-fg :foreground ,golokai-bg))))
   `(fringe ((t (:foreground ,golokai-base02 :background ,golokai-bg))))
   `(highlight ((t (:background ,golokai-grey))))
   `(region ((t (:background  ,golokai-grey+2))
             (t :inverse-video t)))
   `(warning ((t (:foreground ,golokai-palevioletred :weight bold))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,golokai-chartreuse))))
   `(font-lock-comment-face ((t (:foreground ,golokai-base01))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,golokai-base01))))
   `(font-lock-constant-face ((t (:foreground ,golokai-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,golokai-darkgoldenrod))))
   `(font-lock-function-name-face ((t (:foreground ,golokai-chartreuse))))
   `(font-lock-keyword-face ((t (:foreground ,golokai-pink :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,golokai-wine))))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,golokai-darkgoldenrod))))
   `(font-lock-type-face ((t (:foreground ,golokai-blue :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,golokai-orange))))
   `(font-lock-warning-face ((t (:foreground ,golokai-palevioletred :weight bold))))

   ;; mode line
   `(mode-line ((t (:foreground ,golokai-fg
                                :background ,golokai-base03
                                :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-inactive ((t (:foreground ,golokai-fg
                                         :background ,golokai-base02
                                         :box nil))))

   ;; search
   `(isearch ((t (:foreground ,golokai-dark :background ,golokai-yellow :weight bold))))
   `(isearch-fail ((t (:foreground ,golokai-wine :background ,golokai-darkwine))))

   ;; linum-mode
   `(linum ((t (:foreground ,golokai-grey-2 :background ,golokai-grey+10))))

   ;; hl-line-mode
   `(hl-line-face ((,class (:background ,golokai-grey+5)) (t :weight bold)))
   `(hl-line ((,class (:background ,golokai-grey+10)) (t :weight bold)))

   ;; TODO
   ;; ido-mode
   ;; flycheck
   ;; show-paren
   ;; rainbow-delimiters
   ;; highlight-symbols
   ))

(defcustom golokai-theme-kit nil
  "Non-nil means load golokai-theme-kit UI component"
  :type 'boolean
  :group 'golokai-theme)

(defcustom golokai-theme-kit-file
  (concat (file-name-directory
           (or (buffer-file-name) load-file-name))
          "golokai-theme-kit.el")
  "golokai-theme-kit-file"
  :type 'string
  :group 'golokai-theme)

(if (and golokai-theme-kit
         (file-exists-p golokai-theme-kit-file))
    (load-file golokai-theme-kit-file))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'golokai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; golokai-theme.el ends here
