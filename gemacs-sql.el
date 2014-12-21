;;; gemacs-sql.el --- SQL related stuff.
;;
;;; Commentary:
;;
;;; Code:


(add-to-list 'auto-mode-alist
             '("\\.mysql\\'" . (lambda ()
                                 (sql-mode)
                                 (sql-set-product 'mysql))))
(eval-after-load "sql"
  (load-library "sql-indent"))

(provide 'gemacs-sql)

;;; gemacs-sql.el ends here
