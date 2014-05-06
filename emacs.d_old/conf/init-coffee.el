;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; coffee mode
;; https://github.com/defunkt/coffee-mode
;; (auto-install-from-url "https://raw.github.com/defunkt/coffee-mode/master/coffee-mode.el")

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(provide 'init-coffee)
