;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://code.google.com/p/yasnippet/
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/elisp/snippets")
;; (add-to-list 'yas/extra-mode-hooks 'cperl-mode-hook)

;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el")
;; (require 'anything-c-yasnippet)
;; (setq anything-c-yas-space-match-any-greedy t) ;[default: nil]
;; (global-set-key (kbd "C-c y") 'anything-c-yas-complete)

(provide 'init-yasnippet)
