;;; gemacs-neo4j.el --- SQL related stuff.
;;
;;; Commentary:
;;
;;; Code:


(add-to-list 'auto-mode-alist
             '("\\.cyp\\'" . (lambda ()
                                 (cypher-mode)
                                 )))
(require 'cypher-mode)

(provide 'gemacs-neo4j)

;;; gemacs-neo4j.el ends here
