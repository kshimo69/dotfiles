;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://svn.coderepos.org/share/lang/elisp/outputz/outputz.el
;; http://d.hatena.ne.jp/hayamiz/20081121/1227230429
(require 'outputz)
(setq outputz-key "Bby9m2635Q.C")

(setq outputz-base-uri
      (concat "http://%s." (user-login-name) ".org/" (system-name)))
(setq outputz-uri outputz-base-uri)

(global-outputz-mode t)

;; outputzのモード追加
(add-to-list 'outputz-modes 'change-log-mode)
(add-to-list 'outputz-modes 'systemtap-mode)
(add-to-list 'outputz-modes 'markdown-mode)
(add-to-list 'outputz-modes 'howm-mode)
(add-to-list 'outputz-modes 'org-mode)

(provide 'init-outputz)
