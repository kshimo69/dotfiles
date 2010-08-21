;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-from-emacswiki "auto-install.el")

;; AutoInstall
;; http://www.emacswiki.org/emacs/AutoInstall
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'init-autoinstall)
