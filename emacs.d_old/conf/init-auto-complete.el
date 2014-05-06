;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://cx4a.org/software/auto-complete/manual.ja.html
;; (auto-install-batch "auto-complete development version")

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
(ac-config-default)

;; ユーザ辞書
(setq ac-user-dictionary '("kshimo69@gmail.com"
                           "Kimihiko Shimomura"
                           ))

;; 補完メニュー表示時のみC-n/C-pで補完候補を選択
(setq ac-use-menu-map t)
;; デフォルトで設定済み
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; auto-complete-modeを有効にするモードを追加
(add-to-list 'ac-modes 'html-mode)

;; ;; 大文字・小文字を区別しない
;; (setq ac-ignore-case t)
;; 補完対象に大文字が含まれる場合のみ区別する
(setq ac-ignore-case 'smart)
;; ;; 大文字・小文字を区別する
;; (setq ac-ignore-case nil)

;; auto-completeをキーに割りあてておく
;; (global-set-key (kbd "M-/") 'auto-complete)

;; 補完開始までの秒数
;; (setq ac-auto-start 4)

;; auto-startせずにTABキーで補完開始する場合
;; (setq ac-auto-start nil)
;; (ac-set-trigger-key "TAB")

;; (defun ac-emacs-lisp-mode-setup ()
;;   (setq ac-sources (append '(ac-source-features
;;                              ac-source-functions
;;                              ac-source-variables
;;                              ac-source-symbols
;;                              ) ac-sources)))


;; (defun ac-emacs-lisp-mode-setup ()
;;   (setq ac-sources '(ac-source-symbols
;;                      ac-source-words-in-same-mode-buffers
;;                      )))
;; or
;; (defun my-ac-emacs-lisp-mode ()
;;   (setq ac-sources '(ac-source-symbols
;;                      ac-source-words-in-same-mode-buffers
;;                      )))
;; (add-hook 'emacs-lisp-mode-hook 'my-ac-emacs-lisp-mode)

;; ;; 全てのバッファの`ac-sources`の末尾に辞書情報源を追加
;; (defun ac-common-setup ()
;;   (setq ac-sources (append ac-sources '(ac-source-dictionary))))

;; http://d.hatena.ne.jp/kitokitoki/20100627/p1
;; リージョンを開いているバッファのメジャーモードの辞書へ追加する
(defvar auto-complete-dict-path "~/.emacs.d/lisp/auto-complete/dict/")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/dict/")
(defun append-region-to-auto-complete-dict ()
  (interactive)
  (when (and transient-mark-mode mark-active)
    (let ((path (concat auto-complete-dict-path (prin1-to-string major-mode)))
          (str (concat "\n" (buffer-substring-no-properties (region-beginning) (region-end)))))
      (with-temp-buffer
        (insert str)
        (append-to-file (point-min) (point-max) path)))))

(global-set-key (kbd "M-h") 'append-region-to-auto-complete-dict)

;; http://d.hatena.ne.jp/kitokitoki/20110409/p3
(setq ac-source-dictionary
;; '((candidates . ac-dictionary-candidates)
'((candidates . ac-buffer-dictionary)
  (match . substring)
  (symbol . "d")))

(provide 'init-auto-complete)
