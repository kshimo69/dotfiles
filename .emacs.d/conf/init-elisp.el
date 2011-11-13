;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; ;; 括弧の対応を取りながらS式を編集する
;; ;; (auto-install-from-url "http://mumble.net/~campbell/emacs/paredit.el")
;; (require 'paredit)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; Emacs Lisp関数・変数のヘルプをエコーエリアに表示する
;; (auto-install-from-emacswiki "eldoc-extension.el")
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.4)
(setq eldoc-minor-mode-string "")

;; Emacs Lisp式の値をコメントで注釈する
;; (auto-install-from-emacswiki "lispxmp.el")
(require 'lispxmp)

;; ユニットテストを書く
;; (auto-install-batch "el-expectations")
(require 'el-expectations)

(provide 'init-elisp)
