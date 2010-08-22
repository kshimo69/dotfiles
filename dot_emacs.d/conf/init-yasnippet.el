;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; autoinsert
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/autoinsert/")
(define-auto-insert 'python-mode "python.py")
(define-auto-insert 'perl-mode "perl.pl")
(define-auto-insert 'cperl-mode "perl.pl")
(define-auto-insert 'rst-mode "rst.rst")
(define-auto-insert 'c-mode "c.c")

;; http://code.google.com/p/yasnippet/
;; (auto-install-from-emacswiki "yasnippet-config.el")
(require 'yasnippet-config)
(yas/setup "~/.emacs.d/elisp")

;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/elisp/snippets")
;; (add-to-list 'yas/extra-mode-hooks 'cperl-mode-hook)

;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el")
;; (require 'anything-c-yasnippet)
;; (setq anything-c-yas-space-match-any-greedy t) ;[default: nil]
;; (global-set-key (kbd "C-c y") 'anything-c-yas-complete)

(provide 'init-yasnippet)
