;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://openlab.ring.gr.jp/skk/skk-manual/skk-manual-ja_2.html#SEC4

;; migemo
;; http://googlewhacks.blogspot.com/2008/01/migemo.html
(require 'migemo)

(require 'skk-autoloads)

(setq skk-large-jisyo "~/.emacs.d/share/skk/SKK-JISYO.L")
(setq skk-aux-large-jisyo "~/.emacs.d/share/skk/SKK-JISYO.L")
(setq skk-tut-file "~/.emacs.d/share/skk/SKK.tut")

;; ;; SKK使用時のインクリメント検索用設定
;; ;; http://openlab.jp/skk/skk-manual/skk-manual-ja_3.html#SEC7
;; (add-hook 'isearch-mode-hook
;;           #'(lambda ()
;;               (when (and (boundp 'skk-mode)
;;                          skk-mode
;;                          skk-isearch-mode-enable)
;;                 (skk-isearch-mode-setup))))
;; (add-hook 'isearch-mode-end-hook
;;           #'(lambda ()
;;               (when (and (featurep 'skk-isearch)
;;                          skk-isearch-mode-enable)
;;                 (skk-isearch-mode-cleanup))))

;; C-x C-j でSKK
(global-set-key (kbd "C-x C-j") 'skk-mode)

;; C-\ でも SKK に切り替えられるように設定
(setq default-input-method "japanese-skk")

;; C-jの機能を別のキーに割り当て
(global-set-key (kbd "C-m") 'newline-and-indent)

;; macの場合skk serverを見に行く
(when ns-p
  (setq skk-server-host "localhost")
  (setq skk-server-portnum 1178))

;; SKK句読点変更
;; (setq skk-kuten-touten-alist '((jp . ("。" . "，"))))
;; (setq-default skk-kutouten-type 'jp)

;; 変換時，改行でも確定
(setq skk-egg-like-newline t)

;; メッセージは日本語で
(setq skk-japanese-message-and-error t)

;; "「"を入力したら"」"も自動で挿入
(setq skk-auto-insert-paren t)

;; 漢字登録のミスをチェックする
(setq skk-check-okurigana-on-touroku t)

;; 変換候補をツールチップに表示
;; (setq skk-show-tooltip t)

;; 変換候補をインラインに表示
(setq skk-show-inline t)

;; BSを押した時に前の候補を表示する
(setq skk-delete-implies-kakutei t)

;; 10 分放置すると個人辞書が自動的に保存される設定
(defvar skk-auto-save-jisyo-interval 600)
(defun skk-auto-save-jisyo ()
  (skk-save-jisyo)
  (skk-bayesian-save-history)
  (skk-bayesian-corpus-save))
(run-with-idle-timer skk-auto-save-jisyo-interval
                     skk-auto-save-jisyo-interval
                     'skk-auto-save-jisyo)

(provide 'init-skk)
