;;; Configuration specific to Mac OS X and Darwin.

(defun read-system-path ()
  "Reads and parses the `PATH' system environment variable into a
list of paths."
  (with-temp-buffer
    (insert-file-contents "/etc/paths")
    (goto-char (point-min))
    (replace-regexp "\n" ":")
    (thing-at-point 'line)))

;; (setenv "PATH" (read-system-path))

(provide 'darwin)
