;;; gemacs-packages.el --- Packages related stuff.
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(setq
 el-get-sources
 '(el-get
   ;; source based stuff here.
   ))
(setq
 goog:el-get-packages
 '(
   ;; Programming related.
   ack-and-a-half
   anaconda-mode
   clojure-mode
   company-anaconda
   company-c-headers
   company-mode
   go-company
   go-mode
   go-oracle
   js2-mode
   js2-refactor
   racket-mode

   ;; C and C++ related.
   company-irony
   flycheck-irony
   irony-mode
   irony-eldoc

   ;; Java-related
   popup
   eclim

   ;; Utility.
   gist
   diff-hl
   expand-region
  ))
(setq goog:el-get-packages
      (append
       goog:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

(el-get 'sync goog:el-get-packages)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar default-packages '(
                           ;; Programming-related.
                           auctex
                           cider
                           cypher-mode
                           go-autocomplete
                           go-eldoc
                           go-errcheck
                           golint
                           graphviz-dot-mode
                           haskell-mode
                           javadoc-lookup
                           less-css-mode
                           markdown-mode
                           protobuf-mode
                           sass-mode
                           sql-indent
                           tup-mode
                           vimrc-mode
                           yaml-mode
                           zencoding-mode

                           ;; C
                           clang-format

                           ;; Utility.
                           ace-jump-mode
                           exec-path-from-shell
                           fastnav
                           flycheck
                           helm
                           helm-ls-git
                           helm-ls-hg
                           highlight-symbol
                           highlight-unique-symbol
                           ido-ubiquitous
                           iedit
                           key-chord
                           magit
                           multiple-cursors
                           nav
                           persp-mode
                           powerline
                           rainbow-delimiters
                           rcirc-color
                           smartparens
                           smex
                           switch-window
                           undo-tree
                           yasnippet
                           ))
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'gemacs-packages)

;;; gemacs-packages.el ends here
