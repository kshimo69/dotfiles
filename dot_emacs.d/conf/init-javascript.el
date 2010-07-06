;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq js2-indent-on-enter-key t)

(provide 'init-javascript)
