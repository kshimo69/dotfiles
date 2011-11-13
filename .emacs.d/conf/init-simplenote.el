;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://blog.serverworks.co.jp/tech/2010/06/30/emacs-iphone-simplenote-and-vuvuzela/
;; (auto-install-from-url "http://github.com/cefstat/simplenote.el/raw/master/simplenote.el")
(require 'simplenote)
;; (setq simplenote-email "hoge@fuga.com")
;; (setq simplenote-password "password")
(simplenote-setup)

(add-hook 'before-save-hook
          (lambda()
            (when (equal (buffer-name) "*Simplenote*")
              (simplenote-sync-notes)
              (simplenote-browser-refresh))))

(provide 'init-simplenote)
