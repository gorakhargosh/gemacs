;;; c-auto-include.el --- auto include header file for C++

;; Copyright (C) 2014 by Syohei YOSHIDA
;; Copyright (C) 2015 Google Inc.

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/
;; Version: 0.01

;; Author: Yesudeep Mangalapilly <yesudeep@google.com>
;; URL: https://github.com/khargosh/
;; Version: 0.01

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

;;; Code:

(require 'cl-lib)
(require 'rx)

(defvar c-auto-include--header-regexp
  `(("stdio.h" nil t
     ,(rx (and symbol-start
               (or  (and (or "scanf" "sscanf" "puts" "sprintf" "printf"
                             "gets" "fgets" "putchar")
                         (* space) "(")
                    (and  (or "FILE" "stdin" "stdout" "stderr")
                          symbol-end)))))
    ("assert.h" nil t "\\bassert\\s-+(")
    ("string.h" nil t
     ,(rx (and symbol-start
               (or "memcpy" "memset" "memcmp" "memncmp"
                   "strlen" "strcmp" "strncmp" "strcpy" "strncpy" "strerr" "strcat"
                   "strstr" "strchr")
               symbol-end)))
    ("stdlib.h" nil t
     ,(rx (and symbol-start
               (or (and (or "system" "abs" "atoi" "atof" "itoa"
                            "strtod" "strtold" "strtoul" "strtof" "strtol"
                            "strtoll" "strtoull" "strtoq" "strtouq"
                            "free" "exit" "labs" "srand" "srandom" "srandom_r"
                            "rand" "rand_r" "random" "random_r" "qsort")
                        (* space) "(")
                   (and (or (and "EXIT_" (1+ (in "A-Z")))
                            "NULL"))))))
    ("math.h" nil t
     ,(rx (and symbol-start
               (or (and (or "powf" "powl"
                            "acos" "acosf" "acosh" "acoshf" "acoshl" "acosl"
                            "asin" "asinf" "asinh" "asinhf" "asinhl" "asin"
                            "atan" "atan2" "atan2f" "atan2l" "atanf" "atanh" "atanhf"
                            "atanhl" "atanl" "exp" "expf" "expl" "exp10" "exp10f"
                            "exp10l" "exp2" "exp2f" "exp2l" "expm1" "expm1f" "expm1l"
                            "fabs" "fabsf" "fabsl" "log" "logf" "logl"
                            "log2" "log2f" "log2l" "log10" "log10f" "log10l" "log1p"
                            "log1pf" "log1pl" "nan" "nanf" "nanl"
                            "ceil" "ceilf" "ceill" "floor" "floorf" "floorl"
                            "round" "roundf" "roundl" "lround" "lroundf" "lroundl"
                            "llround" "llroundf" "llroundl" "sqrt" "sqrtf" "sqrtl")
                        (* space) "(")
                   (and (or "NAN" "INFINITY" "HUGE_VAL" "HUGE_VALF" "HUGE_VALL")
                        symbol-end)))))
    ("time.h" nil t ,(rx (and symbol-start
                             (or (and (or "time" "clock")
                                      (* space) "(")
                                 (and (or "fixed" "hex")
                                      symbol-end)))))
    ("string" t t "\\bstring\\b")
    ("utility" t t "\\b\\(?:pair\\s-*<\\|make_pair\\)")))

(defun c-auto-include--include-line (header)
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward (concat "<" header ">") nil t)
         (line-number-at-pos))))

(defsubst c-auto-include--in-string-or-comment-p ()
  (nth 8 (syntax-ppss)))

(defun c-auto-include--has-keyword-p (regexp line)
  (save-excursion
    (goto-char (point-min))
    (when line
      (forward-line line))
    (let (finish)
      (while (and (not finish) (re-search-forward regexp nil t))
        (unless (c-auto-include--in-string-or-comment-p)
          (setq finish t)))
      finish)))

(defun c-auto-include--parse-file ()
  (cl-loop with use-std = nil
           with added = nil
           with removed = nil
           with case-fold-search = nil
           for info in c-auto-include--header-regexp
           for header = (nth 0 info)
           for regexp = (nth 3 info)
           for included-line = (c-auto-include--include-line header)
           for has-keyword = (c-auto-include--has-keyword-p regexp included-line)

           when (and (not use-std) has-keyword)
           do (setq use-std t)

           do
           (cond ((and has-keyword (not included-line))
                  (cl-pushnew header added :test 'equal))
                 ((and included-line (not has-keyword))
                  (cl-pushnew (cons header included-line) removed :test 'equal)))

           finally
           return (list :use-std use-std
                        :added added :removed removed)))

(defun c-auto-include--header-files ()
  (save-excursion
    (goto-char (point-min))
    (let ((re "^\\s-*#\\s-*include\\s-*<\\([^>]+\\)>")
          headers)
     (while (re-search-forward re nil t)
       (cl-pushnew (match-string-no-properties 1) headers :test 'equal))
     headers)))

(defun c-auto-include--header-insert-point ()
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^#\\s-*include\\s-*[<\"]" nil t)
      (forward-line 1)
      (point))))

(defun c-auto-include--add-headers (headers)
  (save-excursion
    (let ((insert-point (or (c-auto-include--header-insert-point) (point-min))))
      (goto-char insert-point)
      (dolist (header headers)
        (insert (format "#include <%s>\n" header)))
      (unless (re-search-forward "^\\s-*$" (line-end-position) t)
        (insert "\n")))))

(defun c-auto-include--remove-headers (headers)
  (save-excursion
    (cl-loop with deleted-lines = 0
             initially (goto-char (point-min))
             for (header . line) in (sort headers (lambda (a b) (< (cdr a) (cdr b))))
             for curline = 1 then (line-number-at-pos)
             do
             (progn
               (forward-line (- line curline deleted-lines))
               (let ((beg (point)))
                 (forward-line 1)
                 (delete-region beg (point))
                 (cl-incf deleted-lines))))))

;;;###autoload
(defun c-auto-include ()
  (interactive)
  (let* ((info (c-auto-include--parse-file))
         (added (plist-get info :added))
         (removed (plist-get info :removed)))
    (when removed
      (c-auto-include--remove-headers removed))
    (when added
      (c-auto-include--add-headers added))))

(provide 'c-auto-include)

;;; c-auto-include.el ends here
