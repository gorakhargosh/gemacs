;; (add-hook 'LaTeX-mode-hook
;;           (lambda()
;;              (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;;              (setq TeX-command-default "XeLaTeX")
;;              (setq TeX-save-query nil)
;;              (setq TeX-show-compilation t)))

(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

(provide 'gemacs-tex)
