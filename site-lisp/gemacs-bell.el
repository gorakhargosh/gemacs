;;; gemacs-bell -- Prevents the bell from ringing all the time.
;;
;; URL: http://www.elliotglaysher.org/emacs/
;; Author: Miles Bader <miles /at/ gnu.org>
;;
;;; Commentary:
;;
;; Nice little alternative visual bell
;;
;;; Code:

;; TODO(erg): Figure out why that note doesn't appear in the mode-line-bar...
(defcustom mode-line-bell-string "ding" ; "?"
  "Message displayed in mode-line by `mode-line-bell' function."
  :group 'user)
(defcustom mode-line-bell-delay 0.1
  "Number of seconds `mode-line-bell' displays its message."
  :group 'user)

;; internal variables
(defvar mode-line-bell-cached-string nil)
(defvar mode-line-bell-propertized-string nil)

;;;###autoload
(defun mode-line-bell ()
  "Briefly display a highlighted message in the mode-line.

The string displayed is the value of `mode-line-bell-string',
with a red background; the background highlighting extends to the
right margin.  The string is displayed for `mode-line-bell-delay'
seconds.

This function is intended to be used as a value of `ring-bell-function'."

  (unless (equal mode-line-bell-string mode-line-bell-cached-string)
    (setq mode-line-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(string-width mode-line-bell-string))))
            mode-line-bell-string)
           'face '(:background "black")))
    (setq mode-line-bell-cached-string mode-line-bell-string))
  (message mode-line-bell-propertized-string)
  (sit-for mode-line-bell-delay)
  (message ""))

;;;###autoload
(setq ring-bell-function 'mode-line-bell)

(provide 'gemacs-bell)
;;; gemacs-bell ends here
