;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(autoload 'systemtap-mode "systemtap-mode")
(setq auto-mode-alist (cons '("\\.stp$" . systemtap-mode) auto-mode-alist))

(provide 'init-systemtap)
