;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-from-emacswiki "auto-install.el")

;; AutoInstall
;; http://www.emacswiki.org/emacs/AutoInstall
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name nil)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; EmacsWikiを閲覧、編集する
;; (auto-install-from-emacswiki "yaoddmuse.el")
(require 'yaoddmuse)
;; ページを保存するディレクトリ
(setq yaoddmuse-derectory "~/.emacs.d/yaoddmuse")
;; 書き込み時の名前
(setq yaoddmuse-username "kshimo69")
;; 起動時にページ名のリストを読み込む
(yaoddmuse-update-pagename t)

(provide 'init-autoinstall)
