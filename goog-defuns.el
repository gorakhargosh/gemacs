;;; goog-defuns.el --- Extra functionality.
;;
;;; Commentary:
;;
;;; Code:

(defun turn-on-watchwords ()
  "Turn on watchwords in the buffer."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|NOTE\\|WARNING\\(S\\)?\\|CAUTION\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; ----------------------------------------------------------------------
;; Navigation.
;; ----------------------------------------------------------------------
(defun goto-match-paren (arg)
  "Use ARG to go to the matching parenthesis if on parenthesis.
Otherwise, go to the opening parenthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido.

Params:

SYMBOL-LIST."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol: " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;; ----------------------------------------------------------------------
;; Buffers.
;; ----------------------------------------------------------------------
(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their `major-mode` is equal to MODE."
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

;; ----------------------------------------------------------------------
;; Search.
;; ----------------------------------------------------------------------
(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

;; ----------------------------------------------------------------------
;; Editing.
;; ----------------------------------------------------------------------

;; Line insertion and duplication.
(defun insert-blank-line-below ()
  "Insert blank line below current line."
  (interactive)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun insert-blank-line-above ()
  "Insert blank line above current line."
  (interactive)
  (previous-line 1)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun insert-blank-line-below-next-line ()
  "Insert an empty line below the next line."
  (interactive)
  (next-line 1)
  (move-end-of-line nil)
  (open-line 1)
  (next-line 1))

(defun duplicate-current-line-or-region (arg)
  "Duplicate the current line or region ARG times.
If there's no region, the current line will be duplicated.
However, if there's a region, all lines that region covers will
be duplicated."
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

;; Whitespace and indentation.
(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  "Tabify the entire buffer."
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  "Properly indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun shift-region (distance)
  "Shift the selected region right if DISTANCE is positive.
Left if negative."
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  "Shifts the selection toward the right."
  (interactive)
  (shift-region 1))

(defun shift-left ()
  "Shifts the selection toward the left."
  (interactive)
  (shift-region -1))

;; Killing and yanking.
(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following.
Otherwise kill line deletes whitespace at join.

Param:

ARG."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

;; Transpositions.
(defun transpose-params ()
  "Transposes formal function parameters.

Presumes that params are in the form (p, p, p) or {p, p, p} or
[p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun move-forward-out-of-param ()
  "Move forward out of param."
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  "Move backward out of param."
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

;; Case.
(defun toggle-letter-case ()
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
         ((looking-at "[[:lower:]][[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]")
          (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]")
          (put this-command 'state "init caps") )
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

(defun replace-next-underscore-with-camel (arg)
  "Replace next underscore with camel case.

Param:

ARG."
  (interactive "p")
  (if (> arg 0)
      (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))

(defun toggle-identifier-naming-style ()
  "Toggle the symbol at point between C-style naming.

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


;; TODO(yesudeep): Refactor these in snippets too.
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

(defun goog/edit/camelcase-to-downcased-underscore-string
  (s &optional sep start)
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

(defun goog/edit/camelcase-to-downcased-hyphenated-string
  (s &optional sep start)
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

;; Toggle quotes.
(defun current-quotes-char ()
  "Current quotes char."
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  "Move point forward out of string."
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  "Move point backward out of string."
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  "Alternate quotes char."
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  "Toggle quotes."
  (interactive)
  (if (point-is-in-string-p)
      (let ((old-quotes (char-to-string (current-quotes-char)))
            (new-quotes (char-to-string (alternate-quotes-char)))
            (start (make-marker))
            (end (make-marker)))
        (save-excursion
          (move-point-forward-out-of-string)
          (backward-delete-char 1)
          (set-marker end (point))
          (insert new-quotes)
          (move-point-backward-out-of-string)
          (delete-char 1)
          (insert new-quotes)
          (set-marker start (point))
          (replace-string new-quotes (concat "\\" new-quotes) nil start end)
          (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))

;; Comment what I really really really mean.
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.

If no region is selected and current line is not blank and we are
not at the end of the line, then comment current line.  Replaces
default behaviour of `comment-dwim', when it inserts comment at
the end of the line.

Param:

ARG"
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    (comment-dwim arg)))

;; View.
(defun text-scale-reset ()
  "Reset the text scale back to 0."
  (interactive)
  (text-scale-set 0))

;; Perspectives.
(defun persp-curr-position (offset)
  "Persp at the current OFFSET position."
  (+ offset
     (position (persp-name persp-curr)
               (persp-all-names))))

(defun persp-next ()
  "Switch to next perspective."
  (interactive)
  (persp-switch (nth (persp-curr-position -1) (persp-all-names))))

(defun persp-prev ()
  "Switch to previous perspective."
  (interactive)
  (persp-switch (nth (persp-curr-position +1) (persp-all-names))))

;; http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  "Move text internal."
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line )
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (< arg 0)
            (forward-line -1)))         ;works  in emacs 24.3
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region or current line ARG lines down.

`(transient-mark-mode active)`"
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region or current line ARG lines up.

`(transient-mark-mode active)`"
  (interactive "*p")
  (move-text-internal (- arg)))


(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  "Toggle transparency."
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 70))))

;; Set transparency of emacs
(defun transparency (value)
  "Set the transparency of the frame window to VALUE.

0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


(provide 'goog-defuns)

;;; goog-defuns.el ends here
