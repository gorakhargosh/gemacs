;;; burp-theme.el --- Burp color theme for GNU Emacs 24
;;
;; Copyright 2012 Google Inc. All Rights Reserved.
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
;;
;; Author: Yesudeep Mangalapilly <yesudeep@google.com>
;; Version: 1.0.0

(deftheme burp
  "Burp color theme")

(custom-theme-set-faces
 'burp
 '(default ((t (:background "#1a1a1a" :foreground "#eeeeec"))))
 '(cursor ((t (:foreground "#888888"))))
 '(region ((t (:background "#555753"))))
 '(highlight ((t (:background "#121212"))))
 ;; '(modeline ((t (:background "#2e3436" :foreground "#eeeeec"))))
 ;; '(modeline-inactive ((t (:background "#111111" :foreground "#cccddd"))))
 '(mode-line ((t (:bold t :foreground "#000000" :background "#63a216" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#222222" :box nil))))
 '(fringe ((t (:background "#111111"))))
 '(minibuffer-prompt ((t (:foreground "#729fcf"))))
 '(font-lock-builtin-face ((t (:foreground "#729fcf"))))
 '(font-lock-comment-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-function-name-face ((t (:foreground "#729fcf"))))
 '(font-lock-keyword-face ((t (:foreground "#fcaf3e"))))
 '(font-lock-string-face ((t (:foreground "#73d216"))))
 '(font-lock-type-face ((t (:foreground "#c17d11"))))
 '(font-lock-variable-name-face ((t (:foreground "#fce94f"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))
 '(font-lock-doc-face ((t (:foreground "#888a85"))))
 ;; '(font-lock-doc-face ((t (:foreground "#5aa411")))) ;; A little darker green
 '(link ((t (:foreground "#729fcf"))))
 '(link-visited ((t (:foreground "#ad7fa8"))))

 ;; ----------------------------------------------------------------------
 ;; js2-mode
 ;; ----------------------------------------------------------------------
 ;; Function parameters and their js-doc names should match.
 ;; '(js2-jsdoc-value-face ((t (:foreground "#32cd32"))))
 ;; '(js2-function-param-face ((t (:foreground "#32cd32"))))
 '(js2-jsdoc-value-face ((t (:foreground "#28a428"))))
 '(js2-function-param-face ((t (:foreground "#28a428"))))

 '(js2-jsdoc-tag-face ((t (:foreground "#699bc4"))))
 '(js2-jsdoc-type-face ((t (:foreground "#699bc4"))))

 ;; ----------------------------------------------------------------------
 ;; Search.
 ;; ----------------------------------------------------------------------
 '(isearch ((t (:background "#ffcc00" :foreground "#121212"))))
 '(highlight-symbol-face ((t (:foreground "#ffcc00"))))
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
 ;; TODO(yesudeep): Fix ediff colors.
 )

;; When Emacs is using transparency, darken the background to increase contrast.
(let ((value (nth 1 (frame-parameter nil 'alpha))))
  (if (and value (/= value 100))
      (custom-theme-set-faces
       'burp
       '(default ((t (:background "#000000" :foreground "#eeeeec")))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'burp)

;;; burp-theme.el ends here
