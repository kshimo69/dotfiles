;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'migemo)
;; (setq migemo-command "/usr/bin/ruby")
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)

;; for cmigemo
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "/usr/local/share/migemo/euc-jp/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)

(provide 'init-migemo)
