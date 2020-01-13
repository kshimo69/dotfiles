;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; Memo
;;
;; 変数の設定
;; (setq 変数 値)
;; 条件分岐if
;; (if 条件
;;    正なら
;;    偽なら)
;; 式をまとめる
;; (progn 'g1 'g2 'g3.......)
;; 条件分岐cond
;; (cond
;;      (条件1 式)
;;      (条件2 式)
;;      ........
;;      .......)
;;
;; 文字コードの変更方法
;;
;; キーボードから入力する文字コード
;; C-x RET kの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x set-keyboard-coding-systemと同じ
;;
;; 画面表示に使用する文字コード
;; C-x RET tの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x set-terminal-coding-systemと同じ
;;
;; ファイルの保存に使用する文字コード(カレントバッファのみ)
;; C-x RET fの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x set-buffer-file-coding-systemと同じ
;;
;; バッファやファイルの文字コード(emacs全体で有効)
;; C-x RET cの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x universal-coding-system-argumentと同じ
;;
;; 文字コードを指定して再読み込み
;; C-x RET rの後、ミニバッファの質問に変更したい文字コードを入力する。
;; M-x revert-buffer-with-coding-systemと同じ

;; 常時デバッグ状態
(setq debug-on-error t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Mac用設定
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
(setq grep-find-use-xargs 'bsd)
(setq browse-url-generic-program "open")

;; Ctrl/Cmd/Optionがシステムに渡されるのを防ぐ
(setq mac-pass-control-to-system nil)
(setq mac-pass-command-to-system nil)
(setq mac-pass-option-to-system nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;(eval-when-compile
;  (setq use-package-always-defer t)
;  (setq use-package-enable-imenu-support t)
;  (setq use-package-minimum-reported-time 0)
;  (setq use-package-verbose t)
;  (setq use-package-compute-statistics t)
;  (require 'use-package)
;  (setq use-package-always-ensure t))

(progn ; `use-package'
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (setq use-package-always-ensure t)
  (require 'use-package))
