;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-from-emacswiki "text-translator.el")
;; (auto-install-from-emacswiki "text-translator-vars.el")
;; (auto-install-from-emacswiki "text-translator-load.el")

;; http://d.hatena.ne.jp/khiker/20070503/emacs_text_translator
(require 'text-translator)
(global-set-key (kbd "C-x M-t") 'text-translator)
(global-set-key (kbd "C-x M-T") 'text-translator-translate-last-string)
(setq text-translator-default-engine "google.com_enja")

;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)
;; グローバルキーを設定
(global-set-key (kbd "C-x t") 'text-translator-translate-by-auto-selection)

(provide 'init-texttranslator)
