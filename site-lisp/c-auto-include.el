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

;; TODO(yesudeep): Add POSIX headers.

(require 'cl-lib)
(require 'rx)

(defvar c-auto-include--header-regexp
  `(("stdio.h" nil t
     ,(rx (and symbol-start
               (or  (and (or
                          "clearerr"
                          "fclose"
                          "feof"
                          "ferror"
                          "fflush"
                          "fgetc"
                          "fgetpos"
                          "fgets"
                          "fopen"
                          "fprintf"
                          "fputc"
                          "fputs"
                          "fread"
                          "freopen"
                          "fscanf"
                          "fseek"
                          "fsetpos"
                          "ftell"
                          "fwrite"
                          "getc"
                          "getchar"
                          "gets"
                          "perror"
                          "printf"
                          "putc"
                          "putchar"
                          "puts"
                          "remove"
                          "rename"
                          "rewind"
                          "scanf"
                          "setbuf"
                          "setvbuf"
                          "sprintf"
                          "sscanf"
                          "tmpfile"
                          "tmpnam"
                          "ungetc"
                          "vfprintf"
                          "vprintf"
                          "vsprintf"
                          )
                         (* space) "(")
                    (and  (or
                           "BUFSIZ"
                           "EOF"
                           "FILE"
                           "FILENAME_MAX"
                           "FOPEN_MAX"
                           "L_tmpnam"
                           "SEEK_CUR"
                           "SEEK_END"
                           "SEEK_SET"
                           "TMP_MAX"
                           "_IOFBF"
                           "_IOLBF"
                           "_IONBF"
                           "fpos_t"
                           "stderr"
                           "stdin"
                           "stdout"
                           )
                          symbol-end)))))
    ("assert.h" nil t "\\bassert\\s-+(")
    ("string.h" nil t
     ,(rx (and symbol-start
               (or
                "memchr"
                "memcmp"
                "memcpy"
                "memmove"
                "memncmp"
                "memset"
                "sterror"
                "strcat"
                "strchr"
                "strcmp"
                "strcoll"
                "strcpy"
                "strcspn"
                "strerr"
                "strlen"
                "strncat"
                "strncmp"
                "strncpy"
                "strpbrk"
                "strspn"
                "strstr"
                "strtok"
                "strxfrm"
                )
               symbol-end)))
    ("ctype.h" nil t
     ,(rx (and symbol-start
               (or
                "isalnum"
                "isalpha"
                "iscntrl"
                "isdigit"
                "isgraph"
                "islower"
                "isprint"
                "ispunct"
                "isspace"
                "isupper"
                "isxdigit"
                "tolower"
                "toupper"
                )
               symbol-end)))
    ("stdlib.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "abort"
                         "abs"
                         "atexit"
                         "atof"
                         "atoi"
                         "atol"
                         "bsearch"
                         "calloc"
                         "div"
                         "exit"
                         "free"
                         "getenv"
                         "itoa"
                         "labs"
                         "ldiv"
                         "malloc"
                         "mblen"
                         "mbstowcs"
                         "mbtowc"
                         "qsort"
                         "rand"
                         "rand_r"
                         "random"
                         "random_r"
                         "realloc"
                         "srand"
                         "srandom"
                         "srandom_r"
                         "strtod"
                         "strtof"
                         "strtol"
                         "strtold"
                         "strtoll"
                         "strtoq"
                         "strtoul"
                         "strtoull"
                         "strtouq"
                         "system"
                         "wcstombs"
                         "wctomb"
                         )
                        (* space) "(")
                   (and (or
                         "EXIT_FAILURE"
                         "EXIT_SUCCESS"
                         "MB_CUR_MAX"
                         "RAND_MAX"
                         "div_t"
                         "ldiv_t"
                         ))))))
    ("errno.h" nil t
     ,(rx (and symbol-start
               (and (or
                     "errno"
                     "EDOM"
                     "ERANGE"
                     )
                    symbol-end))))
    ("float.h" nil t
     ,(rx (and symbol-start
               (and (or
                     "DBL_DIG"
                     "DBL_EPSILON"
                     "DBL_MANT_DIG"
                     "DBL_MAX"
                     "DBL_MAX_10_EXP"
                     "DBL_MIN"
                     "DBL_MIN_10_EXP"
                     "DBL_MIN_EXP"
                     "FLT_DIG"
                     "FLT_EPSILON"
                     "FLT_MANT_DIG"
                     "FLT_MAX"
                     "FLT_MAX_10_EXP"
                     "FLT_MIN"
                     "FLT_MIN_10_EXP"
                     "FLT_MIN_EXP"
                     "FLT_RADIX"
                     "FLT_ROUNDS"
                     "LDBL_DIG"
                     "LDBL_EPSILON"
                     "LDBL_MANT_DIG"
                     "LDBL_MAX"
                     "LDBL_MAX_10_EXP"
                     "LDBL_MIN"
                     "LDBL_MIN_10_EXP"
                     "LDBL_MIN_EXP"
                     )
                    symbol-end))))
    ("limits.h" nil t
     ,(rx (and symbol-start
               (and (or
                     "CHAR_BIT"
                     "CHAR_MAX"
                     "CHAR_MIN"
                     "INT_MAX"
                     "INT_MIN"
                     "LONG_MAX"
                     "LONG_MIN"
                     "MB_LEN_MAX"
                     "SCHAR_MAX"
                     "SCHAR_MIN"
                     "SHRT_MAX"
                     "SHRT_MIN"
                     "UCHAR_MAX"
                     "UINT_MAX"
                     "ULONG_MAX"
                     "USHRT_MAX"
                     )
                    symbol-end))))
    ("stddef.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "offsetof"
                         )
                        (* space) "(")
                   (and (or
                         "ptrdiff_t"
                         "size_t"
                         "wchar_t"
                         "NULL"
                         )
                        symbol-end)))))
    ("stdarg.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "va_arg"
                         "va_end"
                         "va_start"
                         )
                        (* space) "(")
                   (and (or
                         "va_list"
                         )
                        symbol-end)))))
    ("signal.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "raise"
                         "signal"
                         )
                        (* space) "(")
                   (and (or
                         "sig_atomic_t"
                         "SIG_DFL"
                         "SIG_ERR"
                         "SIG_IGN"
                         "SIGABRT"
                         "SIGFPE"
                         "SIGILL"
                         "SIGINT"
                         "SIGSEGV"
                         "SIGTERM"
                         )
                        symbol-end)))))
    ("setjmp.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "setjmp"
                         "longjmp"
                         )
                        (* space) "(")
                   (and (or
                         "jmp_buf"
                         )
                        symbol-end)))))
    ("locale.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "localeconv"
                         "setlocale"
                         )
                        (* space) "(")
                   (and (or
                         "LC_ALL"
                         "LC_COLLATE"
                         "LC_CTYPE"
                         "LC_MONETARY"
                         "LC_NUMERIC"
                         "LC_TIME"
                         )
                        symbol-end)))))
    ("math.h" nil t
     ,(rx (and symbol-start
               (or (and (or
                         "acos"
                         "acosf"
                         "acosh"
                         "acoshf"
                         "acoshl"
                         "acosl"
                         "asin"
                         "asin"
                         "asinf"
                         "asinh"
                         "asinhf"
                         "asinhl"
                         "atan"
                         "atan2"
                         "atan2f"
                         "atan2l"
                         "atanf"
                         "atanh"
                         "atanhf"
                         "atanhl"
                         "atanl"
                         "ceil"
                         "ceilf"
                         "ceill"
                         "exp"
                         "exp10"
                         "exp10f"
                         "exp10l"
                         "exp2"
                         "exp2f"
                         "exp2l"
                         "expf"
                         "expl"
                         "expm1"
                         "expm1f"
                         "expm1l"
                         "fabs"
                         "fabsf"
                         "fabsl"
                         "floor"
                         "floorf"
                         "floorl"
                         "llround"
                         "llroundf"
                         "llroundl"
                         "log"
                         "log10"
                         "log10f"
                         "log10l"
                         "log1p"
                         "log1pf"
                         "log1pl"
                         "log2"
                         "log2f"
                         "log2l"
                         "logf"
                         "logl"
                         "lround"
                         "lroundf"
                         "lroundl"
                         "nan"
                         "nanf"
                         "nanl"
                         "powf"
                         "powl"
                         "round"
                         "roundf"
                         "roundl"
                         "sqrt"
                         "sqrtf"
                         "sqrtl"
                         )
                        (* space) "(")
                   (and (or
                         "HUGE_VAL"
                         "HUGE_VALF"
                         "HUGE_VALL"
                         "INFINITY"
                         "NAN"
                         )
                        symbol-end)))))
    ("time.h" nil t ,(rx (and symbol-start
                              (or (and (or
                                        "asctime"
                                        "clock"
                                        "ctime"
                                        "difftime"
                                        "gmtime"
                                        "localtime"
                                        "mktime"
                                        "strftime"
                                        "time"
                                        )
                                       (* space) "(")
                                  (and (or
                                        "CLOCKS_PER_SEC"
                                        "clock_t"
                                        "struct tm"
                                        "time_t"
                                        )
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
