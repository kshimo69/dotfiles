;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; AutoInstall
;; http://www.emacswiki.org/emacs/AutoInstall
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp/")
(auto-install-update-emacswiki-package-name t)

(provide 'init-autoinstall)
