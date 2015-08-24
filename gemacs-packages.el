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
   (:name go-mode
          :website "http://github.com/dominikh/go-mode.el#readme"
          :description "An improved go-mode."
          :type github
          :pkgname "dominikh/go-mode.el"
          :post-init (progn
                       (require 'go-mode)))
   ))
(setq
 goog:el-get-packages
 '(
   auto-complete
   diff-hl
   expand-region
   fill-column-indicator
   js2-mode
   js2-refactor
   ;; magit
   rst-mode
   company-mode
   go-company
   company-c-headers
   anaconda-mode
   company-anaconda
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
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar default-packages '(
                           ;; evil-numbers
                           ;; find-things-fast
                           ;; company-arduino
                           ;; company-auctex
                           ;; company-c-headers
                           ;; company-cabal
                           ;; ;; company-cmake
                           ;; company-ghc
                           ;; company-ghci
                           ;; company-go
                           ;; company-math
                           ;; company
                           ;; company-web
                           ace-jump-mode
                           ack-and-a-half
                           auctex
                           auto-compile
                           cypher-mode
                           dart-mode
                           emacs-eclim
                           exec-path-from-shell
                           fastnav
                           flycheck
                           go-autocomplete
                           go-eldoc
                           go-errcheck
                           golint
                           graphviz-dot-mode
                           haskell-mode
                           helm
                           highlight-symbol
                           highlight-unique-symbol
                           ido-ubiquitous
                           iedit
                           key-chord
                           less-css-mode
                           magit
                           markdown-mode
                           multiple-cursors
                           nav
                           persp-mode
                           powerline
                           protobuf-mode
                           rainbow-delimiters
                           rcirc-color
                           sass-mode
                           smartparens
                           sql-indent
                           switch-window
                           tup-mode
                           undo-tree
                           vimrc-mode
                           yaml-mode
                           yasnippet
                           zencoding-mode

                           smex
                           ))
(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'gemacs-packages)

;;; gemacs-packages.el ends here
