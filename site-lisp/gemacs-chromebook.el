;;; gemacs-chromebook.el --- Chromebook related fixes.
;;
;;; Commentary:
;;
;;; Code:

(defun fix-terminal-keys ()
  (interactive)
  (define-key input-decode-map "\e[5~" [(meta up)])
  (define-key input-decode-map "\e[6~" [(meta down)])
  (define-key input-decode-map "\e\e[OD" [(meta left)])
  (define-key input-decode-map "\e\e[OC" [(meta right)])
  (define-key input-decode-map "\e[A" [(ctrl up)])
  (define-key input-decode-map "\e[B" [(ctrl down)])
  (define-key input-decode-map "\e[C" [(ctrl right)])
  (define-key input-decode-map "\e[D" [(ctrl left)])
  (define-key input-decode-map "\e[1~" [(meta ctrl up)])
  (define-key input-decode-map "\e[4~" [(meta ctrl down)])
  (define-key input-decode-map "\e\e[D" [(meta ctrl left)])
  (define-key input-decode-map "\e[3~" (kbd "M-DEL")))

(advice-add 'terminal-init-xterm :after #'fix-terminal-keys)

;; See: http://www.alex-charlton.com/posts/Using_the_Samsung_Chromebook_for_remote_and_local_development_with_Emacs/

(require 'osc52e)

(provide 'gemacs-chromebook)

;;; gemacs-chromebook.el ends here
