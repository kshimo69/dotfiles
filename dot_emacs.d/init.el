;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://bitbucket.org/sakito/dot.emacs.d
;;
;; Emacs 設定ディレクトリを設定。Emacs 22以下用
;; Emacs 23.1 以上では user-emacs-directory 変数が用意されているのでそれを利用
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中の中で
;; [A-Za-z] で開始する物だけ追加するので、追加したくない物は . や _ を先頭に
;; 付与しておけばロードしない
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path "elisp"
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

;; 安全な実行のための共通系関数
;; @see http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro eval-safe (&rest body)
  "安全な評価。評価に失敗してもそこで止まらない。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))
(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))
(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (if (not (listp functions))
      (setq functions (list functions)))
  (and (locate-library file)
       (progn
         (dolist (function functions)
           (autoload function file docstring interactive type))
         t )))

;; 個別設定のロード
(require 'init-global)
(require 'init-color)
(require 'init-anything)
(require 'init-yasnippet)
(require 'init-autocomplete)
(require 'init-skk)
(require 'init-w3m)
(require 'init-clmemo)
(require 'init-shellpop)
(require 'init-flymake)
(require 'init-perl)
(require 'init-python)
(require 'init-systemtap)
(require 'init-javascript)
(require 'init-gauche)
(require 'init-git)
(require 'init-sdic)
(require 'init-texttranslator)
(require 'init-taskpaper)
(require 'init-gtags)
(require 'init-hatena)
(require 'init-twitter)
(require 'init-lingr)
(require 'init-outputz)
(require 'init-popup)
(require 'init-org)
(require 'init-autoinstall)

;; 環境依存をロード
(cond
 (mac-p (require 'init-mac))
 (carbon-p (require 'init-carbon))
 (linux-p (require 'init-linux))
 )

;; 会社だったらproxyとかの設定
(when (locate-library "init-passwd")
  (require 'init-passwd))

;; 終了時バイトコンパイル
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (if (file-newer-than-file-p (concat user-emacs-directory "init.el")
                                        (concat user-emacs-directory "init.elc"))
                (byte-compile-file (concat user-emacs-directory "init.el")))
            (byte-recompile-directory (concat user-emacs-directory "elisp") 0)
            ;; (byte-recompile-directory (concat user-emacs-directory "local-elisp") 0)
            (byte-recompile-directory (concat user-emacs-directory "conf") 0)
            ))

;; 終了時に聞く
;; (setq confirm-kill-emacs 'y-or-n-p)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((clmemo-mode . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
