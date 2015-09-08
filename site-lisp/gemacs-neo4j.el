;;; gemacs-neo4j.el --- Cypher/neo4j related stuff.
;;
;;; Commentary:
;;
;;; Code:


(add-to-list 'auto-mode-alist
             '("\\.cypher\\|\\.cql\\'" . (lambda ()
                                 (cypher-mode)
                                 )))
(require 'cypher-mode)

(provide 'gemacs-neo4j)

;;; gemacs-neo4j.el ends here
