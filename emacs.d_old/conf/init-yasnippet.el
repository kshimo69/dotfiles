;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; autoinsert
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/share/autoinsert/")
(define-auto-insert 'python-mode "python.py")
(define-auto-insert 'perl-mode "perl.pl")
(define-auto-insert 'cperl-mode "perl.pl")
(define-auto-insert 'ruby-mode "ruby.rb")
(define-auto-insert 'rst-mode "rst.rst")
(define-auto-insert 'c-mode "c.c")

;; http://code.google.com/p/yasnippet/
(require 'yasnippet)

;; (auto-install-from-url "http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el")
(setq yas/root-directory "~/.emacs.d/lisp/yasnippet/snippets")
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t) ;[default: nil]
(global-set-key (kbd "C-c y") 'anything-c-yas-complete)

;; http://code.google.com/p/yasnippet/
(yas/initialize)
(yas/load-directory yas/root-directory)
;; (add-to-list 'yas/extra-mode-hooks 'cperl-mode-hook)

;; ;; (auto-install-from-emacswiki "yasnippet-config.el")
;; (require 'yasnippet-config)
;; (yas/setup "~/.emacs.d/plugins/yasnippet")
;; (global-set-key (kbd "C-x y") 'yas/register-oneshot-snippet)
;; (global-set-key (kbd "C-x C-y") 'yas/expand-oneshot-snippet)

(provide 'init-yasnippet)
