;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://cx4a.org/software/auto-complete/manual.ja.html
;; (auto-install-batch "auto-complete development version")

(require 'auto-complete-config)
(global-auto-complete-mode 1)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)

;; C-n、C-pでも候補選択可能に
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)

;; auto-completeをキーに割りあてておく
;; (global-set-key (kbd "M-/") 'auto-complete)

;; 補完開始までの秒数
;; (setq ac-auto-start 4)

;; auto-startせずにTABキーで補完開始する場合
;; (setq ac-auto-start nil)
;; (ac-set-trigger-key "TAB")

;; auto-complete-modeを有効にするモードを追加
;; (add-to-list 'ac-modes 'hoge-mode)

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features
                             ac-source-functions
                             ac-source-variables
                             ac-source-symbols
                             ) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags
                             ) ac-sources)))

(provide 'init-autocomplete)










