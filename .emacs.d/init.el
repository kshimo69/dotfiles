;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://bitbucket.org/sakito/dot.emacs.d/

;;init.el -- Emacs init setting elisp file

;; http://sakito.jp/emacs/emacs23.html
;; ; 変数の設定
;; (setq 変数 値)
;; ; 条件分岐if
;; (if 条件
;;    正なら
;;    偽なら)
;; ; 式をまとめる
;; (progn 'g1 'g2 'g3.......)
;; ; 条件分岐cond
;; (cond
;;      (条件1 式)
;;      (条件2 式)
;;      ........
;;      .......)

;; Memo
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

;; Emacs 設定ディレクトリを設定。Emacs 22以下用
;; Emacs 23.1 以上では user-emacs-directory 変数が用意されているので
;; それを利用
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中の中で
;; [A-Za-z] で開始する物だけ追加するので、追加したくない物は . や
;; _ を先頭に付与しておけばロードしない
;; dolist は Emacs 21 から標準関数なので積極的に利用して良い
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path "lisp"
                  ;; ELPA packages
                  "elpa"
                  ;; download plugins
                  ;; "plugins"
                  ;; 自作の Emacs Lisp
                  ;; "local-elisp"
                  ;; 初期設定ファイル
                  "conf")

;; Emacs の種類バージョンを判別するための変数を定義
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))
(defvar emacs22-p (equal emacs-major-version 22))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar darwin-p (eq system-type 'darwin))
(defvar ns-p (featurep 'ns))
(defvar carbon-p (and (eq window-system 'mac) emacs22-p))
(defvar mac-p (and (eq window-system 'mac) emacs23-p))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar colinux-p (when linux-p
                    (let ((file "/proc/modules"))
                      (and
                       (file-readable-p file)
                       (x->bool
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (re-search-forward "^cofuse\.+" nil t)))))))
(defvar cygwin-p (eq system-type 'cygwin))
(defvar nt-p (eq system-type 'windows-nt))
(defvar meadow-p (featurep 'meadow))
(defvar windows-p (or cygwin-p nt-p meadow-p))

;; 文字コード
;;(set-language-environment 'Japanese)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
;; 極力UTF-8とする
(cond
 (mac-p
  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は
  ;; NFD (の様な物)で扱う
  ;; 以下はファイル名を NFC で扱う環境と共同作業等する場合の対処
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (ns-p
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (windows-p
  (setq file-name-coding-system 'sjis)
  (setq locale-coding-system 'utf-8))
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

;; 全環境共通設定
(require 'init-global)
(require 'init-elpa)
(require 'init-migemo)
(require 'init-moccur)
(require 'init-color)
(require 'init-anything)
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-popwin)
(require 'init-skk)
(require 'init-windows)
;; (require 'init-mozc)
(require 'init-clmemo)
(require 'init-sdic)
(require 'init-shellpop)
(require 'init-flymake)
(require 'init-elisp)
(require 'init-c)
(require 'init-perl)
(require 'init-python)
(require 'init-ruby)
(require 'init-rails)
(require 'init-systemtap)
(require 'init-javascript)
(require 'init-coffee)
(require 'init-gauche)
(require 'init-git)
(require 'init-mercurial)
(require 'init-text-translator)
;; (require 'init-taskpaper)
(require 'init-gtags)
(require 'init-hatena)
;; (require 'init-lingr)
;; (require 'init-outputz)
(require 'init-simplenote)
(require 'init-rst)
(require 'init-popup)
(require 'init-org)
(require 'init-twitter)
(require 'init-auto-install)
(require 'init-riece)
;; (require 'init-hahhah)

;; 環境依存設定
(cond
 (mac-p (require 'init-mac))
 (carbon-p (require 'init-carbon))
 (ns-p (require 'init-ns))
 (linux-p (require 'init-linux))
 (windows-p (require 'init-nt))
 )

;; 会社だったらproxyとかの設定
(when (locate-library "init-passwd")
  (require 'init-passwd))

;; custom-file
(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load-file custom-file)

;; 終了時バイトコンパイル
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (if (file-newer-than-file-p
                 (concat user-emacs-directory "init.el")
                 (concat user-emacs-directory "init.elc"))
                (byte-compile-file
                 (concat user-emacs-directory "init.el")))
            (byte-recompile-directory
             (concat user-emacs-directory "lisp") 0)
            (byte-recompile-directory
             (concat user-emacs-directory "conf") 0)
            ))

;; 終了時に聞く
(setq confirm-kill-emacs 'y-or-n-p)
