;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://www.emacswiki.org/emacs/TextTranslator
;; (auto-install-batch "text translator")

(require 'text-translator)
(setq text-translator-default-engine "google.com_enja")

(global-set-key (kbd "C-x M-t") 'text-translator)
(global-set-key (kbd "C-x M-T") 'text-translator-translate-last-string)
;; translate all sites.
;; for example, if you specify "enja", text-translator use google.com_enja, yahoo.com_enja, ... .
(global-set-key (kbd "C-x M-a") 'text-translator-all)

;; The trasnlation results show the popup (text overlay).
;; You have to require the popup.el. (http://github.com/m2ym/auto-complete)
(require 'popup)
(setq text-translator-display-popup t)

;; set function that use auto selection
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)
;; set global-key
(global-set-key (kbd "C-x t") 'text-translator-translate-by-auto-selection)

(provide 'init-text-translator)
