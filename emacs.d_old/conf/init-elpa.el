;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; Copyright (C) 2012  Kimihiko Shimomura
;; (auto-install-from-url "http://bit.ly/pkg-el23")

(when (require 'package nil t)
  ;; パッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

(provide 'init-elpa)
;;; init-elpa.el ends here
