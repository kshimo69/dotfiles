;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; color settings.

(global-font-lock-mode t)

;; シンタックスハイライトを有効にする
(setq font-lock-maximum-decoration t)

;; 対応する括弧をハイライト表示
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; いろんな括弧をハイライト表示
;; http://www.fan.gr.jp/~ring/Meadow/meadow.html#mic-paren.el
(autoload 'paren-activate "mic-paren" nil t)
(setq paren-match-face 'bold paren-sexp-mode t)

;; 括弧が
;; マッチした場合の色
(set-face-background 'show-paren-match-face "RoyalBlue1")
(set-face-foreground 'show-paren-match-face "white")
;; マッチしていない場合の色
(set-face-background 'show-paren-mismatch-face "Red")
(set-face-foreground 'show-paren-mismatch-face "white")

;; リージョンをハイライト表示
(setq transient-mark-mode t)

;; 検索でマッチした箇所をハイライト表示
(setq search-highlight t)

;; 対話置換でマッチした箇所をハイライト
(setq query-replace-highlight t)

;; 全角スペース、タブ等の様々な空白文字をハイライト
;; Meadow/memoからもらってきたと思われる。
(defface my-face-b-1 '((t (:background "medium aquamarine"))) nil)
(defface my-face-b-2 '((t (:background "gray"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))))

;; EOB を表示
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; 変更点に色付け
;; (global-highlight-changes-mode t)
;; (setq highlight-changes-visibility-initial-state t)
;; (global-set-key (kbd "M-]") 'highlight-changes-next-change)
;; (global-set-key (kbd "M-[")  'highlight-changes-previous-change)

;; 現在行に色を付ける
(defface my-hl-face '((t (:background "gray20"))) nil)
(defvar my-hl-face 'my-hl-face)
(setq hl-line-face 'my-hl-face)
(setq hl-line-sticky-flag t)
(global-hl-line-mode)
(hl-line-mode 1)

;; face を調査するための関数
;; いろいろ知りたい場合は C-u C-x =
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;; color-thema
;; (setq color-theme-load-all-themes nil)
;; (setq color-theme-libraries nil)
(require 'color-theme)
(load "color-theme-library")
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (cond
      (ns-p
       (color-theme-dark-laptop))
      (linux-p
       ;; (require 'color-theme-ntemacs)
       (color-theme-dark-laptop))
      )))

;; カーソルの色
(add-to-list 'default-frame-alist '(cursor-color . "orange"))
(add-to-list 'initial-frame-alist '(cursor-color . "orange"))
(if window-system
    (progn
      (set-cursor-color "orange")))

;; enable hi-lock mode
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy t)

(provide 'init-color)
