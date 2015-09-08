;;; gemacs-packages.el --- Packages related stuff.
;;
;;; Commentary:
;;
;;; Code:

;; ----------------------------------------------------------------------
;; El-get
;; ----------------------------------------------------------------------
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
   ;; (:name smex              ;; a better (ido-like) M-x
   ;;        :after (progn
   ;;                 (setq smex-save-file "~/.emacs.d/.smex-items")
   ;;                 (global-set-key (kbd "M-x") 'smex)
   ;;                 (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   ;; (:name go-mode
   ;;        :website "http://github.com/dominikh/go-mode.el#readme"
   ;;        :description "An improved go-mode."
   ;;        :type github
   ;;        :pkgname "dominikh/go-mode.el"
   ;;        :post-init (progn
   ;;                     (require 'go-mode)))
   ))
(setq
 goog:el-get-packages
 '(
   ;; Programming related.
   anaconda-mode
   company-anaconda
   company-c-headers
   company-mode
   go-company
   go-mode
   go-oracle
   js2-mode
   js2-refactor

   ;; Utility.
   diff-hl
   expand-region
  ))
(setq goog:el-get-packages
      (append
       goog:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

(el-get 'sync goog:el-get-packages)

;; ----------------------------------------------------------------------
;; package.el
;; ----------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar default-packages '(
                           ;; Programming-related.
                           auctex
                           cypher-mode
                           go-autocomplete
                           go-eldoc
                           go-errcheck
                           golint
                           graphviz-dot-mode
                           haskell-mode
                           less-css-mode
                           markdown-mode
                           protobuf-mode
                           sass-mode
                           sql-indent
                           tup-mode
                           vimrc-mode
                           yaml-mode
                           zencoding-mode

                           ;; Utility.
                           ace-jump-mode
                           ack-and-a-half
                           exec-path-from-shell
                           fastnav
                           flycheck
                           ;; helm
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
