;;; cloud-theme.el --- Cloud color theme for GNU Emacs 24
;; Author: Yesudeep Mangalapilly
;; Version: 1.0.0
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(deftheme cloud
  "Cloud color theme")

(custom-theme-set-faces
 'cloud
 '(default ((t (:background "#eeeeec" :foreground "#1a1a1a"))))
 '(cursor ((t (:foreground "#888888"))))
 '(region ((t (:background "#ffe476"))))
 '(highlight ((t (:background "#d3d7cf"))))

 ;; The defaults seem good here for powerline.
 ;; '(mode-line ((t (:foreground "#000000" :background "#5aa411" :box nil))))
 ;; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#555555" :box nil))))
 ;; '(fringe ((t (:background "#111111"))))

 '(minibuffer-prompt ((t (:foreground "#3465a4"))))
 '(font-lock-builtin-face ((t (:foreground "#729fcf"))))
 '(font-lock-comment-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-function-name-face ((t (:bold :foreground "#000000"))))
 '(font-lock-keyword-face ((t (:foreground "#a36f9d"))))
 '(font-lock-string-face ((t (:foreground "#4e9a06"))))
 '(font-lock-type-face ((t (:foreground "#c17d11"))))
 '(font-lock-variable-name-face ((t (:foreground "#888a85"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))
 '(font-lock-doc-face ((t (:foreground "#888a85"))))
 '(link ((t (:foreground "#729fcf"))))
 '(link-visited ((t (:foreground "#ad7fa8"))))

 ;; ----------------------------------------------------------------------
 ;; Custom faces.
 ;; ----------------------------------------------------------------------
 '(font-lock-operator-face ((t (:foreground "#888a85"))))

 ;; ----------------------------------------------------------------------
 ;; js2-mode
 ;; ----------------------------------------------------------------------
 ;; Function parameters and their js-doc names should match.
 '(js2-jsdoc-value-face ((t (:foreground "#555753"))))
 '(js2-function-param-face ((t (:foreground "#888a85"))))
 '(js2-external-variable-face ((t (:foreground "#ce5c00"))))
 '(js2-jsdoc-tag-face ((t (:foreground "SteelBlue"))))
 '(js2-jsdoc-type-face ((t (:foreground "SteelBlue"))))

 ;; ----------------------------------------------------------------------
 ;; Search.
 ;; ----------------------------------------------------------------------
 '(isearch ((t (:background "#ffcc00" :foreground "#121212"))))
 '(highlight-symbol-face ((t (:foreground "#d8ad00"))))
 ;; '(highlight-symbol-face ((t (:inherit isearch))))
 '(lazy-highlight ((t (:inherit isearch))))
 '(isearch-lazy-highlight-face ((t (:inherit isearch))))

 ;; ----------------------------------------------------------------------
 ;; Flyspell.
 ;; ----------------------------------------------------------------------
 '(flyspell-duplicate ((t (:foreground "#fcaf3e"))))
 '(flyspell-incorrect ((t (:foreground "#cc0000"))))

 '(org-date ((t (:foreground "LightSteelBlue" :underline t))))
 '(org-hide ((t (:foreground "#2e3436"))))
 '(org-todo ((t (:inherit font-lock-keyword-face :bold t))))
 '(org-level-1 ((t (:inherit font-lock-function-name-face))))
 '(org-level-2 ((t (:inherit font-lock-variable-name-face))))
 '(org-level-3 ((t (:inherit font-lock-keyword-face))))
 '(org-level-4 ((t (:inherit font-lock-string-face))))
 '(org-level-5 ((t (:inherit font-lock-constant-face))))

 '(comint-highlight-input ((t (:italic t :bold t))))
 '(comint-highlight-prompt ((t (:foreground "#8ae234"))))
 '(paren-face-match ((t (:inherit show-paren-match-face))))
 '(paren-face-match-light ((t (:inherit show-paren-match-face))))
 '(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
 '(persp-selected-face ((t (:foreground "#729fcf"))))
 '(show-paren-match-face ((t (:background "#729fcf" :foreground "#eeeeec"))))
 '(show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))

 ;; ----------------------------------------------------------------------
 ;; Diff mode.
 ;; ----------------------------------------------------------------------
 '(diff-added ((t (:foreground "#4e9a06"))))
 '(diff-changed ((t (:foreground "#edd400"))))
 '(diff-removed ((t (:foreground "#ef2929"))))
 '(diff-header ((t (:foreground "#729fcf"))))
 '(diff-file-header ((t (:foreground "#729fcf"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cloud)

;;; cloud-theme.el ends here
