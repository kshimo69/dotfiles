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

;; Cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)
;; Emacs の種類バージョンを判別するための変数を定義
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))
(defvar emacs22-p (equal emacs-major-version 22))
(defvar emacs23-p (equal emacs-major-version 23))
(defvar emacs24-p (equal emacs-major-version 24))
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
(add-to-load-path
 ;; download plugins
 "plugins"
 ;; ELPA packages
 ;; "elpa"
 ;; 自作の Emacs Lisp
 ;; "local-elisp"
 ;; 初期設定ファイル
 ;; "conf"
 )

;; user設定
(setq user-full-name "Kimihiko Shimomura")
(setq user-mail-address "kshimo69@gmail.com")

;; 初期位置
(cd "~/")

;; exec-path、PATH、MANPATHの追加 (下が優先)
;; http://sakito.jp/emacs/emacsshell.html
(dolist (dir (list
              "/sbin"
              "/bin"
              "/usr/sbin"
              "/usr/bin"
              "/usr/local/sbin"
              "/usr/local/bin"
              "/opt/local/sbin"
              "/opt/local/bin"
              (expand-file-name "~/local/bin")
              (expand-file-name "~/bin")
              ))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))
(setenv "MANPATH"
        (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man"
                (getenv "MANPATH")))

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)

;; scratch のメッセージを空にする
(setq initial-scratch-message nil)

;; emacsclient を利用するためにサーバ起動
;; サーバが起動していた場合は先に起動していた方を優先
(if window-system
    (progn
      (require 'server)
      (when (and (functionp 'server-running-p)
                 (not (server-running-p)))
        (server-start))
      ;; (unless (server-running-p) (server-start))
      (defun skt:raise-frame()
        ;; Frame を前面にする
        (raise-frame (selected-frame))
        ;; キーボードフォーカスを選択しているFrameにする
        (x-focus-frame (selected-frame)))
      (add-hook 'server-visit-hook 'skt:raise-frame)
      (add-hook 'find-file-hook 'skt:raise-frame)
      ))

;; メニューバー、ツールバー、スクロールバー非表示
;; (menu-bar-mode nil)
;; (tool-bar-mode nil)
(tool-bar-mode -1)
;; (scroll-bar-mode nil)
(scroll-bar-mode -1)

;; ヴィジブルベルを抑制
(setq visible-bell nil)

;; ビープ音を抑制
(setq ring-bell-function '(lambda ()))

;; カーソルの点滅を抑制
;; (blink-cursor-mode 0)

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)

;; %の代わりに全体の行数を表示する
(setcar mode-line-position
        '(:eval (format "%d" (count-lines (point-max) (point-min)))))

;; 時刻を表示
(setq display-time-string-forms
      ;; '(24-hours ":" minutes " " month "/" day "(" dayname ")"))
      '(24-hours ":" minutes))
(display-time)

;; 曜日表示は英語
(setq system-time-locale "C")

;; 行番号を表示
;; http://macemacsjp.sourceforge.jp/index.php?CocoaEmacs#aae602ba
(global-linum-mode t)

;; バックアップしない
;; (setq make-backup-files nil)

;; 自動保存したファイルを削除する。
;; (setq delete-auto-savefiles t)

;; 自動セーブしない。
;; (setq auto-save-default nil)

;; ファイルを編集した場合コピーにてバックアップする
;; inode 番号を変更しない
(setq backup-by-copying t)
;; バックアップファイルの保存位置指定
;; !path!to!file-name~ で保存される
(setq backup-directory-alist
      '(
        ("." . "~/.emacs.d/var/backup")
        ))
;; バックアップファイルリストの保存位置変更
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-")

;; TAB はスペース 4 個ぶんを基本
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;; スクロール行数（一行ごとのスクロール）
(setq vertical-centering-font-regexp ".*")
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;; 個人用infoディレクトリを追加
(when (require 'info nil t)
  ;; 全体のinfo優先
  ;; (setq Info-default-directory-list
  ;;       (cons (expand-file-name "~/.emacs.d/info/")
  ;;             Info-default-directory-list))
  ;; 個人用infoディレクトリを優先
  (setq Info-default-directory-list
        (append Info-default-directory-list
                (list (expand-file-name "~/.emacs.d/info"))))
  )

;; ファイル名の1階層上を表示する
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; *で囲まれたバッファ名は対象外
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  )

;; シンボリックファイルを開く時にいちいち聞かない
(setq vc-follow-symlinks t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; yes or no を y or n に
(fset 'yes-or-no-p 'y-or-n-p)

;; default to unified diffs
(setq diff-switches "-u")

;; 行末の空白をめだたせる M-x delete-trailing-whitespaceで削除出来る
(when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))

;; ファイルの最後には \n
(setq require-final-newline t)

;; BSで選択範囲を消す
(delete-selection-mode 1)

;; マウスで選択するとコピーする Emacs 24 ではデフォルトが nil
(setq mouse-drag-copy-region t)

;; C-u C-SPC C-SPC... でカーソル位置を辿る
;; http://d.hatena.ne.jp/kbkbkbkb1/20111205/1322988550
(setq set-mark-command-repeat-pop t)

;; カーソル位置のファイル名、URLで開く
(ffap-bindings)

;; GUI上のコピー
(setq x-select-enable-primary t)

;; kill-ringに同じ内容の文字列を入れない
;; http://d.hatena.ne.jp/kitokitoki/20100515/p1
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; Emacsを終了してもファイルを編集してた位置やminibuffer への入力内容を覚えておく
(when (require 'session nil t)
  (setq session-save-file (expand-file-name "~/.emacs.d/var/session"))
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  ;; これがないと file-name-history に500個保存する前に max-string に達する
  (setq session-globals-max-string 100000000)
  ;; デフォルトでは30!
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize))

;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;; (setq savehist-file (expand-file-name "~/.emacs.d/var/history"))
(setq savehist-file "~/.emacs.d/var/history")
;; savehistのファイルに保存する履歴からfile-name-historyをのぞく
(setq savehist-ignored-variables '(file-name-history))

;; ファイル内のカーソル位置を記録する
(require 'saveplace nil t)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/var/emacs-places")

;; ログの記録行数を減らす
(setq message-log-max 10000)

;; ミニバッファを再帰的に呼び出せるようにする
;; (setq enable-recursive-minibuffers t)

;; *scratch* バッファを消さないように
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

;; 会社だったらproxyとかの設定
(when (locate-library "init-passwd")
  (require 'init-passwd))

;; custom-file
(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load-file custom-file)

;; ;; 終了時バイトコンパイル
;; (add-hook 'kill-emacs-query-functions
;;           (lambda ()
;;             (if (file-newer-than-file-p
;;                  (concat user-emacs-directory "init.el")
;;                  (concat user-emacs-directory "init.elc"))
;;                 (byte-compile-file
;;                  (concat user-emacs-directory "init.el")))
;; ;;            (byte-recompile-directory
;; ;;             (concat user-emacs-directory "lisp") 0)
;; ;;            (byte-recompile-directory
;; ;;             (concat user-emacs-directory "conf") 0)
;;             ))

;; 終了時に聞く
(setq confirm-kill-emacs 'y-or-n-p)

;;;
;;; plugins
;;;

;;; ファイル保存時に自動的にバイトコンパイルする
(when (require 'auto-async-byte-compile nil t)
  ;; 自動バイトコンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "~/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  )

;;; Auto Complete
(when (require 'auto-complete-config nil t)
  (ac-config-default)

  ;; ユーザ辞書
  (setq ac-user-dictionary '("kshimo69@gmail.com"
                             "Kimihiko Shimomura"
                             ))

  ;; historyの保存先
  (setq ac-comphist-file (expand-file-name "~/.emacs.d/var/ac-comphist.dat"))

  ;; 補完メニュー表示時のみC-n/C-pで補完候補を選択
  (setq ac-use-menu-map t)
  ;; デフォルトで設定済み
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)

  ;; auto-complete-modeを有効にするモードを追加
  (add-to-list 'ac-modes 'text-mode 'html-mode)

  ;; 大文字・小文字を区別しない
  ;; (setq ac-ignore-case t)
  ;; 補完対象に大文字が含まれる場合のみ区別する
  (setq ac-ignore-case 'smart)
  ;; 大文字・小文字を区別する
  ;; (setq ac-ignore-case nil)

  ;; auto-completeをキーに割りあてておく
  ;; (global-set-key (kbd "M-/") 'auto-complete)

  ;; 補完開始までの秒数
  ;; (setq ac-auto-start 4)

  ;; auto-startせずにTABキーで補完開始する場合
  ;; (setq ac-auto-start nil)
  ;; (ac-set-trigger-key "TAB")

  ;; http://d.hatena.ne.jp/kitokitoki/20100627/p1
  ;; リージョンを開いているバッファのメジャーモードの辞書へ追加する
  (defvar auto-complete-dict-path "~/.emacs.d/share/ac-dict/")
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/share/ac-dict/")
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
  )

;;; AutoInstall
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/plugins/")
  (setq auto-install-use-wget nil)
  ;; (auto-install-update-emacswiki-package-name t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

;;; git
(when (require 'magit nil t)
  (global-set-key (kbd "C-c s") 'magit-status)
  )
(when (require 'gist nil t)
  (setq gist-view-gist t)
  )

;;; browse-kill-ring
(when (require 'browse-kill-ring nil t)
  ;; (global-set-key (kbd "C-c k") 'browse-kill-ring)
  (browse-kill-ring-default-keybindings) ; M-y
  )

;;; migemo
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)

  ;; for cmigemo
  (setq migemo-command "cmigemo")
  ;; (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/share/migemo/utf-8/migemo-dict"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)

  (load-library "migemo")
  (migemo-init)
  )

;;; 変更箇所にジャンプする
(when (require 'goto-chg nil t)
  (global-set-key (kbd "<f7>") 'goto-last-change)
  (global-set-key (kbd "S-<f7>") 'goto-last-change-reverse)
  )

;;; 使い捨てのファイルを開く
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S.")
  )

;;; カーソル位置を戻す
(when (require 'point-undo nil t)
  (global-set-key (kbd "<f6>") 'point-undo)
  (global-set-key (kbd "S-<f6>") 'point-redo)
  )

;;; popwin
(when (require 'popwin nil t)
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  (setq display-buffer-function 'popwin:display-buffer)
  ;; (setq special-display-function 'popwin:special-display-popup-window)
  (setq popwin:popup-window-height 0.4)

  ;; popwinで表示するバッファ
  (setq anything-samewindow nil)
  (setq popwin:special-display-config
        (append '(
                  ;; ("*anything*" :height 20)
                  ("*Compile-Log*" :height 20 :noselect t)
                  (dired-mode :position top)
                  ("*Org Agenda*")
                  (" *Agenda Commands*")
                  ("*Backtrace*")
                  ("*sdic*" :noselect t :height 20)
                  (" *auto-async-byte-compile*" :height 20 :noselect t)
                  ("*interpretation*")
                  ;; ("\\*hg command" :regexp t :noselect)
                  ("*grep*\\*" :regexp t)
                  (twittering-mode :position top)
                  ("*Packages*")
                  ("^\*helm .+\*$" :regexp t)
                  )
                popwin:special-display-config))
  (define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)
  (define-key dired-mode-map "o" #'(lambda ()
                                     (interactive)
                                     (popwin:find-file
                                      (dired-get-file-for-visit))))
  )

;;; recentf
;; 保存先変更
(set-default 'recentf-save-file "~/.emacs.d/var/recentf")
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-auto-cleanup 30)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 300 t 'recentf-save-list))
  ;; 保存ファイルの設定に リモートファイル tramp の先等を追加。
  ;; これを実施すると起動時にパスワード等の確認はされない
  (add-to-list 'recentf-keep 'file-remote-p)
  (add-to-list 'recentf-keep 'file-readable-p)
  ;; 除外ファイル
  (setq recentf-exclude
        '("\\.elc$"
          "\\.pyc$"
          "\\.cache$"
          "recentf$"
          "revive$"
          "/.emacs.d/var/"
          ))
  ;; Emacs 終了時に cleanup
  (add-hook 'kill-emacs-query-functions 'recentf-cleanup)
  (recentf-mode 1)
)
;; 履歴一覧を開く
(when (require 'recentf-ext nil t)
  (global-set-key (kbd "C-x f") 'recentf-open-files)
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("/GTAGS$"))
  )

;;; revive
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(setq revive:configuration-file (expand-file-name "~/.emacs.d/var/revive"))
;; C-x Sで保存 C-x Fで復元 C-x KでKill
(global-set-key (kbd "C-x S") 'save-current-configuration)
(global-set-key (kbd "C-x F") 'resume)
(global-set-key (kbd "C-x K") 'wipe)
;; 終了時に保存
(add-hook 'kill-emacs-hook 'save-current-configuration)

;; ;;; 使わないバッファを自動的に消す
;; ;; (auto-install-from-emacswiki "tempbuf.el")
;; (when (require 'tempbuf nil t)
;;   (setq tempbuf-life-extension-ratio 100)
;;   ;; ファイルを開いたら自動的にtempbufを有効にする
;;   (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;;   ;; diredバッファに対してtempbufを有効にする
;;   (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
;;   )

;;; gtags
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'gtags-mode-hook
          '(lambda ()
             ;; Local customization (overwrite key mapping)
             ;; (define-key gtags-mode-map (kbd "C-f") 'scroll-up)
             ;; (define-key gtags-mode-map (kbd "C-b") 'scroll-down)
             ;; (define-key gtags-mode-map (kbd "M-t") 'gtags-find-tag)
             ;; (define-key gtags-mode-map (kbd "M-r") 'gtags-find-rtag)
             ;; (define-key gtags-mode-map (kbd "M-s") 'gtags-find-symbol)
             ;; (define-key gtags-mode-map (kbd "M-g") 'gtags-find-with-grep)
             ;; (define-key gtags-mode-map (kbd "C-t") 'gtags-pop-stack)
))
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))
(setq gtags-prefix-key (kbd "C-x g"))
(setq gtags-suggested-key-mapping t)
(setq gtags-auto-update t)
(setq gtags-path-style 'relative)
;; (setq gtags-read-only t)

;; update GTAGS
(defun update-gtags (&optional prefix)
  (interactive "P")
  (let ((rootdir (gtags-get-rootpath))
        (args (if prefix "-v" "-iv")))
    (when rootdir
      (let* ((default-directory rootdir)
             (buffer (get-buffer-create "*update GTAGS*")))
        (save-excursion
          (set-buffer buffer)
          (erase-buffer)
          (let ((result (process-file "gtags" nil buffer nil args)))
            (if (= 0 result)
                (message "update GTAGS error with exit status %d" result))))))))
;; (add-hook 'after-save-hook 'update-gtags)

;; 自動で gtags-mode になるように＆補完リスト作成
(add-hook 'c-mode-hook
          '(lambda ()
             (gtags-mode 1)
             ))
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             ))
(add-hook 'c++-mode-hook
          '(lambda()
             (gtags-mode 1)
             ))
;; gtags not support python
;; (add-hook 'python-mode-hook
;;           '(lambda()
;;              (gtags-mode 1)
;;              ))

;; /usr/includeを参照しつつ別のパスでコードを書く場合
;; $ cd /usr/include
;; $ gtags ~/.include_gtags
;; $ export GTAGSROOT=/usr/include #gtagsを実行したパスを設定
;; $ export GTAGSDBPATH=/.include_gtags #GTAGSなどのインデックスファイルが存在するパスを設定

;;; コマンド連続実行時の動作を変える
(when (require 'sequential-command-config nil t)
  (sequential-command-setup-keys)
  )

;;; ブックマーク
;; ブックマークファイル変更
(set-default 'bookmark-default-file "~/.emacs.d/var/emacs.bmk")
;; ブックマークを変更したら即保存する
(setq bookmark-save 1)
;; ブックマークの使い方
;; ブックマークの設定
;; C-x r m (bookmark-set)
;; ブックマーク選択メニューの表示
;; C-x r l (bookmark-bmenu-list)
;; 超整理法
(progn
  (setq bookmark-sort-flag nil)
  (defun bookmark-arrange-latest-top ()
    (let ((latest (bookmark-get-bookmark bookmark)))
      (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
    (bookmark-save))
  (add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top))

;;; dired
;; 今日変更したファイルを色付け
;; http://masutaka.net/chalow/2011-12-17-1.html
(defface dired-todays-face '((t (:foreground "green"))) nil)
(defvar dired-todays-face 'dired-todays-face)
(defconst month-name-alist
  '(("1"  . "Jan") ("2"  . "Feb") ("3"  . "Mar") ("4"  . "Apr")
    ("5"  . "May") ("6"  . "Jun") ("7"  . "Jul") ("8"  . "Aug")
    ("9"  . "Sep") ("10" . "Oct") ("11" . "Nov") ("12" . "Dec")))
(defun dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
   (let ((month-name
          (cdr (assoc (format-time-string "%b") month-name-alist))))
     (if month-name
         (format
          (format-time-string
           "\\(%Y-%m-%d\\|%b %e\\|%%s %e\\) [0-9]....") month-name)
       (format-time-string
        "\\(%Y-%m-%d\\|%b %e\\) [0-9]....")))
   arg t))
(eval-after-load "dired"
  '(font-lock-add-keywords
    'dired-mode
    (list '(dired-today-search . dired-todays-face))))
;; C-x j で dired-jump
(global-set-key (kbd "C-x j") 'dired-jump)

;;; 現在の関数名を画面の上に表示する
(defun toggle-which-func-mode ()
  (interactive)
  (which-func-mode)
  (if which-func-mode
      (progn
        (delete (assoc 'which-func-mode mode-line-format)
                mode-line-format)
        (setq-default header-line-format
                      '(which-func-mode ("" which-func-format)))
        (set-face-foreground 'which-func "pink")
        )
    (setq-default header-line-format nil)))

;;;
;;; grep
;;;
;; カーソル位置の単語でgrep-find
;; (defun grep-find-current-word ()
;;   (interactive)
;;   (let ((command "find . -type f -print0 | xargs -0 grep -n -i "))
;;     (ffap-copy-string-as-kill)
;;     (grep-find (format "%s %s"
;;                        command (car kill-ring)))))
;; (global-set-key (kbd "C-c g") 'grep-find-current-word)

;; igrep - 対話的にgrepする
(when (require 'igrep nil t)
;; ;; lgrepに-0u8オプションをつけると出力がutf-8になる
;; (igrep-define lgrep (igrep-use-zgrep nil)(ignore-regex-option "-n -0u8"))
;; (igrep-find-define lgrep (igrep-use-zgrep nil)(ignore-regex-option "-n -0u8"))
  )

;; 複数のgrepバッファを使う
(when (require 'grep-a-lot nil t)
  (grep-a-lot-setup-keys)
  ;; ;; igrep用
  ;; (grep-a-lot-advise igrep)
  )

;; grepの検索結果を編集する
;; (auto-install-from-emacswiki "grep-edit.el")
(when (require 'grep-edit nil t))

;; color-moccur
(when (require 'color-moccur nil t)
  ;; M-oをoccur-by-moccurに
  (global-set-key (kbd "M-o") 'occur-by-moccur)
  ;; C-M-oをmoccur-grep-findに
  (global-set-key (kbd "C-M-o") 'moccur-grep-find)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索で除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (add-to-list 'dmoccur-exclusion-mask "\\~$")
  (add-to-list 'dmoccur-exclusion-mask "\\.svn\\/\*")
  ;; (auto-install-from-emacswiki "moccur-edit.el")
  (when (require 'moccur-edit nil t))
  ;; MigemoがあればMigemoを使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t))
  )

;; ag
(when (require 'ag nil t)
  (custom-set-variables
   '(ag-highlight-search t)  ; 検索結果の中の検索語をハイライトする
   '(ag-reuse-window 'nil)   ; 現在のウィンドウを検索結果表示に使う
   '(ag-reuse-buffers 'nil)) ; 現在のバッファを検索結果表示に使う
  (when (require 'wgrep-ag nil t)
    (autoload 'wgrep-ag-setup "wgrep-ag")
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)
    ;; agの検索結果バッファで"r"で編集モードに。
    ;; C-x C-sで保存して終了、C-x C-kで保存せずに終了
    (define-key ag-mode-map (kbd "r") 'wgrep-change-to-wgrep-mode)
    )
  ;; キーバインドを適当につけておくと便利。"\C-xg"とか
  (global-set-key (kbd "C-c g") 'ag)
  ;; ;; ag開いたらagのバッファに移動するには以下をつかう
  ;; (defun my/filter (condp lst)
  ;;   (delq nil
  ;;         (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
  ;; (defun my/get-buffer-window-list-regexp (regexp)
  ;;   "Return list of windows whose buffer name matches regexp."
  ;;   (my/filter #'(lambda (window)
  ;;                  (string-match regexp
  ;;                                (buffer-name (window-buffer window))))
  ;;              (window-list)))
  ;; (global-set-key (kbd "C-c g")
  ;;                 #'(lambda ()
  ;;                     (interactive)
  ;;                     (call-interactively 'ag)
  ;;                     (select-window ; select ag buffer
  ;;                      (car (my/get-buffer-window-list-regexp "^\\*ag ")))))
  )

;; http://sleepboy-zzz.blogspot.jp/2013/10/emacsaghelmhelm-ag-r_4267.html
;; agがあればgrepのかわりに使う
(when (and (require 'grep nil t)
           (require 'thingatpt nil t))
  ;; Use ag if the command was exist, otherwise use grep
  (defvar grep-command-before-query
    (if (zerop (shell-command "which ag"))
        "ag --nogroup -a -S "
      ;; Recursive grep by -r
      "grep -nH -r -e "))

  (defun grep-default-command ()
    (lexical-let*
        ((grep-read-from-point
          (let ((grep-command-before-target
                 (concat grep-command-before-query
                         (shell-quote-argument (grep-tag-default)))))
            (cons (if buffer-file-name
                      (concat grep-command-before-target
                              " *."
                              (file-name-extension buffer-file-name))
                    (concat grep-command-before-target " ."))
                  (1+ (length grep-command-before-target))))))
      grep-read-from-point))

  (setq grep-command (cons (concat grep-command-before-query "\"\" .")
                           (+ (length grep-command-before-query) 2)))
  )

;;;
;;; flymake / flycheck
;;;

;;; markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.txt" . markdown-mode) auto-mode-alist))

;;;
;;; anything / helm
;;;
(when (require 'helm-config nil t)
  (helm-mode 1)
  (setq helm-buffer-max-length 50)

  ;; ;; tramp で remote-directory を開いているときに、helm-for-files を起動すると反応が悪い
  ;; ;; 原因は helm-source-files-in-current-dir だったので、この情報源の指定を削除する
  ;; (setq helm-for-files-preferred-list
  ;;       '(helm-source-buffers-list
  ;;         helm-source-recentf
  ;;         helm-source-bookmarks
  ;;         helm-source-file-cache
  ;;         ;; helm-source-files-in-current-dir
  ;;         helm-source-locate))

  ;; ;; ミニバッファで C-k 入力時にカーソル以降を削除する（C-u C-k でも同様の動きをする）
  ;; (setq helm-delete-minibuffer-contents-from-point t)

  ;; ;; 自動補完を無効にする
  ;; (setq helm-ff-auto-update-initial-value nil)

  ;; C-h でバックスペースと同じように文字を削除できるようにする
  (define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

  ;; ;; プレフィックスキーを C-; に設定する
  ;; (custom-set-variables '(helm-command-prefix-key "C-;"))

  ;; キーバインドを設定する。コマンド起動後は、以下のキーが利用可能となる
  ;;  ・M-n     ：カーソル位置の単語を検索パターンに追加
  ;;  ・C-z     ：チラ見
  ;;  ・C-c C-f ：helm-follow-mode の ON/OFF
  ;; (global-set-key (kbd "C-x C-b") 'helm-for-files)
  ;; (global-set-key (kbd "C-x C-;") 'helm-for-files)
  ;; (define-key helm-command-map (kbd "C-;") 'helm-resume)
  ;; (define-key helm-command-map (kbd "y")   'helm-show-kill-ring)
  ;; (define-key helm-command-map (kbd "o")   'helm-occur)
  ;; (define-key helm-command-map (kbd "C-s") 'helm-occur-from-isearch)
  ;; (define-key helm-command-map (kbd "g")   'helm-do-grep) ; C-u 付で起動すると、recursive となる
  ;; (define-key helm-command-map (kbd "t")   'helm-gtags-find-tag)

  (global-set-key (kbd "C-;") 'helm-mini)
  (global-set-key (kbd "C-x C-l") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; helmコマンドで migemo を有効にする
  (when (require 'helm-migemo nil t)
    (setq helm-migemize-command-idle-delay helm-idle-delay)
    ;; (helm-migemize-command helm-for-files)
    )

  ;; ag
  (when (require 'helm-ag nil t)
    (setq helm-ag-base-command "ag --nocolor --nogroup")
    (global-set-key (kbd "C-M-g") 'helm-ag)
    )

  ;; helpのdescbinds
  (when (require 'helm-descbinds nil t)
    (global-set-key (kbd "C-x C-h b") 'helm-descbinds)
    )

  ;; ;; helm-occur コマンドの起動時に helm-maybe-use-default-as-input（helm コマンドに :input パラメータが
  ;; ;; 指定されていなければ、:default の値を使って表示を更新する）を設定する
  ;; (defadvice helm-occur (around ad-helm-occur activate)
  ;;   (let ((helm-maybe-use-default-as-input t))
  ;;     ad-do-it))

  ;; ;; 情報源 helm-source-occur と helm-source-grep について、利用開始時点から helm-follow-mode を ON にする
  ;; ;; 情報源 helm-source-locate と helm-source-grep について、検索必要最低文字数を 2 とする。
  ;; ;; helm-occur コマンドを使う際に migemo でマッチした箇所がハイライトするようにする
  ;; (add-hook 'helm-before-initialize-hook
  ;;           (lambda ()
  ;;             (when helm-source-locate
  ;;               ;; (setcdr (assq 'candidate-number-limit helm-source-locate) helm-candidate-number-limit)
  ;;               (setcdr (assq 'requires-pattern helm-source-locate) 2))
  ;;             (when helm-source-occur
  ;;               (helm-attrset 'follow 1 helm-source-occur)
  ;;               (delete '(nohighlight) helm-source-occur))
  ;;             (when helm-source-grep
  ;;               (helm-attrset 'follow 1 helm-source-grep)
  ;;               ;; (setcdr (assq 'candidate-number-limit helm-source-grep) helm-candidate-number-limit)
  ;;               (setcdr (assq 'requires-pattern helm-source-grep) 2))))

  ;; http://d.hatena.ne.jp/a666666/20100221/1266695355
  ;; エラーを抑制する対策（エラーが発生した際に設定してみてください）
  ;; (setq max-lisp-eval-depth 5000)
  ;; (setq max-specpdl-size 5000)

  ;; ;; helm-delete-minibuffer-contents-from-point（ミニバッファで C-k 入力時にカーソル以降を
  ;; ;; 削除する)を設定すると、pattern 文字入力後に action が表示されない症状が出ることの対策
  ;; (defadvice helm-select-action (around ad-helm-select-action activate)
  ;;   (let ((helm-delete-minibuffer-contents-from-point nil))
  ;;     ad-do-it))

  ;; ;; helm と elscreen を一緒に使う際に helm の helm-follow-mode を使うと、カーソル制御が
  ;; ;; おかしくなることの対策
  ;; (defadvice helm (around ad-helm-for-elscreen activate)
  ;;   (let ((elscreen-screen-update-hook nil))
  ;;     ad-do-it))

  (cond
   (windows-p
    ;; w32-ime-buffer-switch-p が t の場合に、ミニバッファで漢字を使えるようにする対策
    (setq w32-ime-buffer-switch-p t) ; バッファ切り替え時にIME状態を引き継ぐ
    (defadvice helm (around ad-helm-for-w32-ime activate)
      (let ((select-window-functions nil)
            (w32-ime-composition-window (minibuffer-window)))
        ad-do-it))

    ;; UNC や Tramp のパスに対して、helm-reduce-file-name が正しく機能しないことの対策
    ;; （ (helm-mode 1) として dired を動かした際に C-l（helm-find-files-down-one-level）
    ;; 　が正しく機能するようにする対策）
    (defadvice helm-reduce-file-name (around ad-helm-reduce-file-name activate)
      (let ((fname (ad-get-arg 0))
            (level (ad-get-arg 1)))
        (while (> level 0)
          (setq fname (expand-file-name (concat fname "/../")))
          (setq level (1- level)))
        (setq ad-return-value fname)))

    ;; ffap を使っていて find-file-at-point を起動した場合に、カーソル位置の UNC が正しく
    ;; 取り込まれないことの対策
    (defadvice helm-completing-read-default-1 (around ad-helm-completing-read-default-1 activate)
      (if (listp (ad-get-arg 4))
          (ad-set-arg 4 (car (ad-get-arg 4))))
      ;; (cl-letf (((symbol-function 'regexp-quote)
      (letf (((symbol-function 'regexp-quote)
              (symbol-function 'identity)))
        ad-do-it))

    ;; w32-symlinks を使っている場合に C-u 付きで helm-do-grep を起動すると、選択したファイルを
    ;; no conversion で開いてしまうことの対策
    (defadvice find-file (around ad-find-file activate)
      (let ((current-prefix-arg nil))
        ad-do-it))
    )  ;; windows-p
   )

  )

;;; SKK
(when (require 'skk-autoloads nil t)
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
  )

;;; org-mode
(when (require 'org-install nil t)
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (setq org-agenda-start-with-clockreport-mode t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c b") 'org-iswitchb)
  (global-set-key (kbd "C-c r") 'org-remember)
  (global-set-key (kbd "C-c c") 'org-capture)
  (add-hook 'org-mode-hook '(lambda ()
                              (define-key org-mode-map (kbd "C-c C-p")
                                'outline-previous-visible-heading)
                              (define-key org-mode-map (kbd "C-c C-n")
                                'outline-next-visible-heading)
                              ))
  (setq org-directory "~/org/")
  (setq org-agenda-files (list "~/org/agenda.org"
                               "~/org/code-reading.org"
                               "~/org/mobileorg.org"
                               ))
  ;; (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (concat org-directory "agenda.org"))
  ;; (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  ;; (setq hl-line-face 'underline)

  ;; http://unknownplace.org/archives/orgmode-meets-blosxom.html
  ;; バッファ内のコードブロッックをそのコード用のモードと同じ色でハイライト
  (setq org-src-fontify-natively t)
  ;; カーソルをコードに合わせて C-c ' するとそのモードで編集バッファが立ちあがる

  ;; TODO keywords as workflow state
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "CALENDAR(c)"
                    "SOMEDAY(s)" "REFERENCE(r)" "PROJECT(p)"
                    "|" "DONE(d!)")
          (sequence "NEW(n)" "ASSIGNED(a!)" "|" "FIXED(f!)")
          ))
  ;; record DONE time
  (setq org-log-done 'time)
  ;; repeat task
  (require 'org-habit nil t)

  ;; show TODOs
  (setq org-agenda-custom-commands
        '(("x" "Unscheduled TODO" tags-todo "-SCHEDULED=>\"<now>\"" nil)))

  ;; http://d.hatena.ne.jp/rubikitch/20100819/org
  (setq org-capture-templates
        '(("n" "Note" entry
           (file+headline nil "Note")
           "** %?\n   Added: %T\n   %i\n")
          ("t" "Todo" entry
           (file+headline nil "Tasks")
           "** TODO %?\n   Added: %T\n   %a\n   %i\n")
          ("b" "Bug" entry
           (file+headline nil "Tasks")
           "** NEW %?   :bug:\n   Added: %T\n   %a\n   %i\n")
          ("i" "Idea" entry
           (file+headline nil "New Ideas")
           "** SOMEDAY %?\n   Added: %T\n   %i\n")
          ("d" "Daily review" entry
           (file+headline nil "Tasks")
           "** TODO Daily Review[/] :review:\n%?   DEADLINE: %t\n%[~/org/daily_review.txt]")
          ("w" "Weekly review" entry
           (file+headline nil "Tasks")
           "** TODO Weekly Review %T[/] :review:\n%?%[~/org/weekly_review.txt]")
          ))

  ;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
  ;; http://d.hatena.ne.jp/tomoya/20090309/1236588957
  (org-remember-insinuate)
  (setq org-remember-templates
        '(("Note" ?n "** %^{Brief Description} %^g\n%?\n%i\nAdded: %T" nil "Note")
          ("Bug" ?b "** NEW %^{Brief Description} :bug%^g\n%?\n%i\n%a\nAdded: %T" nil "Tasks")
          ("Idea" ?i "** SOMEDAY %^{Brief Description} %^g\n%?\n%i\nAdded: %T" nil "New Ideas")
          ("Todo" ?t "** TODO %^{Brief Description} %^g\n%?\n%i\nAdded: %T" nil "Tasks")
          ("Daily review" ?d "** TODO Daily Review[/] :review:\n%?   DEADLINE: %t\n%[~/org/daily_review.txt]\n" nil "Tasks")
          ("Weekly review" ?w "** TODO Weekly Review %T[/] :review:\n%?%[~/org/weekly_review.txt]\n" nil "Tasks")
          ))

  (defvar org-code-reading-software-name nil)
  (defvar org-code-reading-file "code-reading.org")
  (defun org-code-reading-read-software-name ()
    (set (make-local-variable 'org-code-reading-software-name)
         (read-string "Code Reading Software: "
                      (or org-code-reading-software-name
                          (file-name-nondirectory
                           (buffer-file-name))))))

  (defun org-code-reading-get-prefix (lang)
    (concat "[" lang "]"
            "[" (org-code-reading-read-software-name) "]"))
  (defun org-remember-code-reading ()
    (interactive)
    (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
           (org-remember-templates
            `(("CodeReading" ?r "** %(identity prefix)%^{Brief Description}\n   %?\n   %a\n   %T"
               ,org-code-reading-file "Memo"))))
      (org-remember)))
  (global-set-key (kbd "C-x m") 'org-remember-code-reading)
  )

;;;
;;; key-bind
;;;

;; C-hをバックスペースに
(global-set-key (kbd "C-h") 'delete-backward-char)
;; mini-bufferとかどこでも効くように
;; (keyboard-translate ?\C-h ?\C-?)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "<f1>") 'help-for-help)

;;改行後に自動インデント
(global-set-key (kbd "C-m") 'newline-and-indent)

;; Scroll buffer without moving the cursor
(global-set-key (kbd "M-p") '(lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") '(lambda () (interactive) (scroll-up 1)))

;; C-q をプリフィックスキー化
(global-set-key (kbd "C-q") (make-sparse-keymap))

;; ;; quoted-insert は C-q C-q へ割り当て
;; (global-set-key (kbd "C-q C-q") 'quoted-insert)

;; window-resizer は C-q C-r (resize) で
(global-set-key (kbd "C-q C-r") 'window-resizer)

;; C-x o にはもううんざり
(global-set-key (kbd "C-q l") 'windmove-right)
(global-set-key (kbd "C-q h") 'windmove-left)
(global-set-key (kbd "C-q j") 'windmove-down)
(global-set-key (kbd "C-q k") 'windmove-up)

;; C-q C-qでウインドウ分割 or 移動
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-q C-q") 'other-window-or-split)

;; カーソル位置の単語をコピー
(global-set-key (kbd "M-c") 'ffap-copy-string-as-kill)

;; ;; http://d.hatena.ne.jp/suztomo/20081123/1227466198
;; ;; isearch-forwardした時にカーソル位置の単語の先頭に移動する
;; (defun isearch-forward-with-heading ()
;;   "Search the word your cursor looking at."
;;   (interactive)
;;   (command-execute 'backward-word)
;;   (command-execute 'isearch-forward))
;; (global-set-key "\C-s" 'isearch-forward-with-heading)

;; 範囲指定してない時にC-wで前の単語を削除
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))
;; minibuffer用
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;; fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    ))
(global-set-key (kbd "M-<RET>") 'toggle-fullscreen)

;; 起動時に縦分割
;; (setq w (selected-window))
;; (split-window w nil t)

;; ;; C-+とC--でフォントサイズを変える
;; ;; http://emacs-fu.blogspot.com/2008/12/zooming-inout.html
;; (defun djcb-zoom (n)
;;   "with positive N, increase the font size, otherwise decrease it"
;;   (set-face-attribute 'default (selected-frame) :height
;;                       (+ (face-attribute 'default :height)
;;                          (* (if (> n 0) 1 -1) 10))))
;; (global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
;; (global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
;; (global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
;; (global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))

;; 略語展開、補完を行うコマンドをまとめる
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially   ;ファイル名の一部
        try-complete-file-name             ;ファイル名全体
        ;; try-expand-all-addrevs             ;静的略語展開
        try-expand-dabbrev                 ;動的略語展開(カレントバッファ)
        try-expand-dabbrev-all-buffers     ;動的略語展開(全バッファ)
        try-expand-dabbrev-from-kill       ;動的略語展開(キルリング)
        try-complete-lisp-symbol-partially ;Lispシンボルの一部
        try-complete-lisp-symbol           ;Lispシンボル全体
        ))
(global-set-key (kbd "M-/") 'hippie-expand)

;; スクリプトを実行する
(global-set-key (kbd "C-c p") 'executable-interpret)


;;;
;;; color
;;;

;; color-theme
(when (require 'color-theme nil t)
  ;; (color-theme-solarized-dark)
  ;; (color-theme-solarized-light)
  ;; (color-theme-initialize)
  (color-theme-molokai)
  )

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
;; (set-face-background 'show-paren-match-face "RoyalBlue1")
;; (set-face-foreground 'show-paren-match-face "white")
;; マッチしていない場合の色
;; (set-face-background 'show-paren-mismatch-face "Red")
;; (set-face-foreground 'show-paren-mismatch-face "white")

;; リージョンをハイライト表示
(setq transient-mark-mode t)

;; 検索でマッチした箇所をハイライト表示
(setq search-highlight t)

;; 対話置換でマッチした箇所をハイライト
(setq query-replace-highlight t)

;; 全角スペース、タブ等の様々な空白文字をハイライト
;; Meadow/memoからもらってきたと思われる。
(defface my-face-b-1 '((t (:background "RoyalBlue3"))) nil)
(defface my-face-b-2 '((t (:background "dim gray"))) nil)
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
;; (defface my-hl-face '((t (:background "gray20"))) nil)
;; (defvar my-hl-face 'my-hl-face)
;; (setq hl-line-face 'my-hl-face)
;; (setq hl-line-sticky-flag t)
(global-hl-line-mode)
(hl-line-mode 1)

;; face を調査するための関数
;; いろいろ知りたい場合は C-u C-x =
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;; カーソルの色
;; (add-to-list 'default-frame-alist '(cursor-color . "orange"))
;; (add-to-list 'initial-frame-alist '(cursor-color . "orange"))
;; (if window-system
;;     (progn
;;       (set-cursor-color "orange")))

;; enable hi-lock mode
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy t)






;;;
;;; OS依存の設定
;;;
(cond
 ;; Macの時の設定
 (darwin-p
  ;; 透明
  ;; (set-frame-parameter nil 'alpha 80)
  (set-frame-parameter (selected-frame) 'alpha '(80 50))

  ;; Mac用設定
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  (setq grep-find-use-xargs 'bsd)
  (setq browse-url-generic-program "open")

  ;; Ctrl/Cmd/Optionがシステムに渡されるのを防ぐ
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-option-to-system nil)

  ;; 文字コード
  ;;(set-language-environment 'Japanese)
  (set-language-environment  'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8-unix)

  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は
  ;; NFD (の様な物)で扱う
  ;; 以下はファイル名を NFC で扱う環境と共同作業等する場合の対処
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  ;; inline-patch
  ;;(mac-add-key-passed-system 'shift)

  ;; バックスラッシュの入力
  (define-key global-map [?¥] [?\\])

  ;; ドラッグ&ドロップした時は新しくファイルを開く
  (define-key global-map [ns-drag-file] 'ns-find-file)

  ;; http://d.hatena.ne.jp/suztomo/20080923/1222149517
  ;; fullscreen
  ;; (when (eq window-system 'ns)
  ;;   (add-hook 'window-setup-hook
  ;;             (lambda ()
  ;;               ;; (set-frame-parameter nil 'fullscreen 'fullboth)
  ;;               (ns-toggle-fullscreen)
  ;;               )))
  ;; (global-set-key (kbd "M-<RET>") 'ns-toggle-fullscreen)

  ;; フォントの設定
  (set-face-attribute 'default nil
                      ;; :family "monaco"
                      :family "Ricty Discord for Powerline"
                      :height 140)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Ricty Discord for Powerline" . "iso10646-1"))
  ;; '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Ricty Discord for Powerline" . "iso10646-1"))
  ;; '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("Ricty Discord for Powerline" . "iso10646-1"))
  ;; '("monaco" . "iso10646-1"))
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          (".*osaka-medium.*" . 1.2)
          (".*courier-bold-.*-mac-roman" . 1.0)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*monaco-bold-.*-mac-roman" . 0.9)
          ("-cdac$" . 1.3)))

  ;; コンパイル時の文字コード
  (add-hook 'compilation-filter-hook
            '(lambda ()
               ;; シェルモードの入出力文字コード
               (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
               (set-buffer-file-coding-system    'utf-8-unix)
               ))
  )
 ;; Windowsの時の設定
 (windows-p
  ;; ------------------------------------------------------------------------
  ;; @ coding system

  ;; 日本語入力のための設定
  (set-language-environment "Japanese")
  (set-terminal-coding-system 'utf-8-unix)
  (set-keyboard-coding-system 'utf-8-unix)
  (set-buffer-file-coding-system 'utf-8-unix)
  ;; (setq default-buffer-file-coding-system 'utf-8-unix)
  (setq buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  ;; コマンドプロンプトを使うならsjis
  (setq file-name-coding-system 'shift_jis)
  (setq locale-coding-system 'shift_jis)  ;; term内の文字コード
  ;; (setq file-name-coding-system 'utf-8)
  ;; (setq locale-coding-system 'utf-8)

  ;; (set-keyboard-coding-system 'cp932)
  ;; (prefer-coding-system 'utf-8-dos)
  ;; (set-file-name-coding-system 'cp932)
  ;; (setq default-process-coding-system '(cp932 . cp932))

  ;; ------------------------------------------------------------------------
  ;; @ encode

  ;; 機種依存文字
  (when (require 'cp5022x nil t)
    (define-coding-system-alias 'euc-jp 'cp51932)
    )

  ;; decode-translation-table の設定
  (coding-system-put 'euc-jp :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'iso-2022-jp :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'utf-8 :decode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

  ;; encode-translation-table の設定
  (coding-system-put 'euc-jp :encode-translation-table
                     (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'iso-2022-jp :encode-translation-table
                     (get 'japanese-ucs-cp932-to-jis-map 'translation-table))
  (coding-system-put 'cp932 :encode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))
  (coding-system-put 'utf-8 :encode-translation-table
                     (get 'japanese-ucs-jis-to-cp932-map 'translation-table))

  ;; charset と coding-system の優先度設定
  (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
                        'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
  (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

  ;; PuTTY 用の terminal-coding-system の設定
  (apply 'define-coding-system 'utf-8-for-putty
         "UTF-8 (translate jis to cp932)"
         :encode-translation-table
         (get 'japanese-ucs-jis-to-cp932-map 'translation-table)
         (coding-system-plist 'utf-8))
  (set-terminal-coding-system 'utf-8-for-putty)

  ;; East Asian Ambiguous
  (defun set-east-asian-ambiguous-width (width)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (dolist (range
               '(#x00A1 #x00A4 (#x00A7 . #x00A8) #x00AA (#x00AD . #x00AE)
                        (#x00B0 . #x00B4) (#x00B6 . #x00BA) (#x00BC . #x00BF)
                        #x00C6 #x00D0 (#x00D7 . #x00D8) (#x00DE . #x00E1) #x00E6
                        (#x00E8 . #x00EA) (#x00EC . #x00ED) #x00F0
                        (#x00F2 . #x00F3) (#x00F7 . #x00FA) #x00FC #x00FE
                        #x0101 #x0111 #x0113 #x011B (#x0126 . #x0127) #x012B
                        (#x0131 . #x0133) #x0138 (#x013F . #x0142) #x0144
                        (#x0148 . #x014B) #x014D (#x0152 . #x0153)
                        (#x0166 . #x0167) #x016B #x01CE #x01D0 #x01D2 #x01D4
                        #x01D6 #x01D8 #x01DA #x01DC #x0251 #x0261 #x02C4 #x02C7
                        (#x02C9 . #x02CB) #x02CD #x02D0 (#x02D8 . #x02DB) #x02DD
                        #x02DF (#x0300 . #x036F) (#x0391 . #x03A9)
                        (#x03B1 . #x03C1) (#x03C3 . #x03C9) #x0401
                        (#x0410 . #x044F) #x0451 #x2010 (#x2013 . #x2016)
                        (#x2018 . #x2019) (#x201C . #x201D) (#x2020 . #x2022)
                        (#x2024 . #x2027) #x2030 (#x2032 . #x2033) #x2035 #x203B
                        #x203E #x2074 #x207F (#x2081 . #x2084) #x20AC #x2103
                        #x2105 #x2109 #x2113 #x2116 (#x2121 . #x2122) #x2126
                        #x212B (#x2153 . #x2154) (#x215B . #x215E)
                        (#x2160 . #x216B) (#x2170 . #x2179) (#x2190 . #x2199)
                        (#x21B8 . #x21B9) #x21D2 #x21D4 #x21E7 #x2200
                        (#x2202 . #x2203) (#x2207 . #x2208) #x220B #x220F #x2211
                        #x2215 #x221A (#x221D . #x2220) #x2223 #x2225
                        (#x2227 . #x222C) #x222E (#x2234 . #x2237)
                        (#x223C . #x223D) #x2248 #x224C #x2252 (#x2260 . #x2261)
                        (#x2264 . #x2267) (#x226A . #x226B) (#x226E . #x226F)
                        (#x2282 . #x2283) (#x2286 . #x2287) #x2295 #x2299 #x22A5
                        #x22BF #x2312 (#x2460 . #x24E9) (#x24EB . #x254B)
                        (#x2550 . #x2573) (#x2580 . #x258F) (#x2592 . #x2595)
                        (#x25A0 . #x25A1) (#x25A3 . #x25A9) (#x25B2 . #x25B3)
                        (#x25B6 . #x25B7) (#x25BC . #x25BD) (#x25C0 . #x25C1)
                        (#x25C6 . #x25C8) #x25CB (#x25CE . #x25D1)
                        (#x25E2 . #x25E5) #x25EF (#x2605 . #x2606) #x2609
                        (#x260E . #x260F) (#x2614 . #x2615) #x261C #x261E #x2640
                        #x2642 (#x2660 . #x2661) (#x2663 . #x2665)
                        (#x2667 . #x266A) (#x266C . #x266D) #x266F #x273D
                        (#x2776 . #x277F) (#xE000 . #xF8FF) (#xFE00 . #xFE0F)
                        #xFFFD
                        ))
        (set-char-table-range table range width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table)))
  (set-east-asian-ambiguous-width 2)

  ;; emacs-w3m
  (eval-after-load "w3m"
    '(when (coding-system-p 'cp51932)
       (add-to-list 'w3m-compatible-encoding-alist '(euc-jp . cp51932))))

  ;; Gnus
  (eval-after-load "mm-util"
    '(when (coding-system-p 'cp50220)
       (add-to-list 'mm-charset-override-alist '(iso-2022-jp . cp50220))))

  ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
  (eval-after-load "mcs-20"
    '(when (coding-system-p 'cp50220)
       (add-to-list 'mime-charset-coding-system-alist
                    '(iso-2022-jp . cp50220))))

  ;; 全角チルダ/波ダッシュをWindowsスタイルにする
  (let ((table (make-translation-table-from-alist '((#x301c . #xff5e))) ))
    (mapc
     (lambda (coding-system)
       (coding-system-put coding-system :decode-translation-table table)
       (coding-system-put coding-system :encode-translation-table table)
       )
     '(utf-8 cp932 utf-16le)))

  ;; ------------------------------------------------------------------------
  ;; @ font

  ;; (set-face-font 'default "Ricty Diminished for Powerline-12")
  (set-face-font 'default "Migu 1M-10")

  ;; ------------------------------------------------------------------------
  ;; @ frame

  ;; cp932エンコード時の表示を「P」とする
  (coding-system-put 'cp932 :mnemonic ?P)
  (coding-system-put 'cp932-dos :mnemonic ?P)
  (coding-system-put 'cp932-unix :mnemonic ?P)
  (coding-system-put 'cp932-mac :mnemonic ?P)

  ;; 透明
  (set-frame-parameter (selected-frame) 'alpha '(95 90))

  ;; ------------------------------------------------------------------------
  ;; @ image-library
  ;; (setq image-library-alist
  ;;       '((xpm "libxpm.dll")
  ;;         (png "libpng14.dll")
  ;;         (jpeg "libjpeg.dll")
  ;;         (tiff "libtiff3.dll")
  ;;         (gif "libungif4.dll")
  ;;         (svg "librsvg-2-2.dll")
  ;;         (gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
  ;;         (glib "libglib-2.0-0.dll")
  ;;         (gobject "libgobject-2.0-0.dll"))
  ;;       )

  ;; ------------------------------------------------------------------------
  ;; @ print

  (setq ps-print-color-p t
        ps-lpr-command "gswin32c.exe"
        ps-multibyte-buffer 'non-latin-printer
        ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
        printer-name nil
        ps-printer-name nil
        ps-printer-name-option nil
        ps-print-header nil          ; ヘッダの非表示
        )

  ;; ------------------------------------------------------------------------
  ;; @ tabbar

  ;; (require 'tabbar)

  ;; ;; tabbar有効化
  ;; (tabbar-mode)

  ;; ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
  ;; (tabbar-mwheel-mode -1)

  ;; ;; タブグループを使用（t：有効，nil：無効）
  ;; (setq tabbar-buffer-groups-function nil)

  ;; ;; ボタン非表示
  ;; (dolist (btn '(tabbar-buffer-home-button
  ;;                tabbar-scroll-left-button
  ;;                tabbar-scroll-right-button))
  ;;   (set btn (cons (cons "" nil) (cons "" nil))))

  ;; ;; タブ表示 一時バッファ一覧
  ;; (defvar tabbar-displayed-buffers
  ;;   '("*scratch*" "*Messages*" "*Backtrace*" "*Colors*"
  ;;     "*Faces*" "*Apropos*" "*Customize*" "*shell*" "*Help*")
  ;;   "*Regexps matches buffer names always included tabs.")

  ;; ;; 作業バッファの一部を非表示
  ;; (setq tabbar-buffer-list-function
  ;;       (lambda ()
  ;;         (let* ((hides (list ?\  ?\*))
  ;;                (re (regexp-opt tabbar-displayed-buffers))
  ;;                (cur-buf (current-buffer))
  ;;                (tabs (delq
  ;;                       nil
  ;;                       (mapcar
  ;;                        (lambda (buf)
  ;;                          (let ((name (buffer-name buf)))
  ;;                            (when (or (string-match re name)
  ;;                                      (not (memq (aref name 0) hides)))
  ;;                              buf)))
  ;;                        (buffer-list)))))
  ;;           (if (memq cur-buf tabs)
  ;;               tabs
  ;;             (cons cur-buf tabs)))))

  ;; ;; キーバインド設定
  ;; (global-set-key (kbd "<C-tab>")   'tabbar-forward-tab)
  ;; (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

  ;; ;; タブ表示欄の見た目（フェイス）
  ;; (set-face-attribute 'tabbar-default nil
  ;;                     :background "SystemMenuBar")

  ;; ;; 選択タブの見た目（フェイス）
  ;; (set-face-attribute 'tabbar-selected nil
  ;;                     :foreground "red3"
  ;;                     :background "SystemMenuBar"
  ;;                     :box (list
  ;;                           :line-width 1
  ;;                           :color "gray80"
  ;;                           :style 'released-button)
  ;;                     :overline "#F3F2EF"
  ;;                     :weight 'bold
  ;;                     :family "ＭＳ Ｐゴシック"
  ;;                     )

  ;; ;; 非選択タブの見た目（フェイス）
  ;; (set-face-attribute 'tabbar-unselected nil
  ;;                     :foreground "black"
  ;;                     :background "SystemMenuBar"
  ;;                     :box (list
  ;;                           :line-width 1
  ;;                           :color "gray80"
  ;;                           :style 'released-button)
  ;;                     :overline "#F3F2EF"
  ;;                     :family "ＭＳ Ｐゴシック"
  ;;                     )

  ;; ;; タブ間隔の調整
  ;; (set-face-attribute 'tabbar-separator nil
  ;;                     :height 0.1)

  ;; ------------------------------------------------------------------------
  ;; @ setup-cygwin
  (setq cygwin-mount-cygwin-bin-directory
        (concat (getenv "CYGWIN_PATH") "\\bin"))
  (require 'setup-cygwin)
  (file-name-shadow-mode -1)

  ;; ------------------------------------------------------------------------
  ;; @ shell
  (require 'shell)
  (setq explicit-shell-file-name "bash.exe")
  (setq shell-command-switch "-c")
  (setq shell-file-name "bash.exe")

  ;; (M-! and M-| and compile.el)
  (setq shell-file-name "bash.exe")
  (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

  ;; shellモードの時の^M抑制
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

  ;; shell-modeでの補完 (for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

  ;; エスケープシーケンス処理の設定
  (autoload 'ansi-color-for-comint-mode-on "ansi-color"
    "Set `ansi-color-for-comint-mode' to t." t)

  (setq shell-mode-hook
        (function
         (lambda ()

           ;; シェルモードの入出力文字コード
           (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
           (set-buffer-file-coding-system    'sjis-unix)
           ;; (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
           ;; (set-buffer-file-coding-system    'utf-8-unix)
           )))

  ;; ------------------------------------------------------------------------
  ;; @ compile
  (setq compile-command "make -j8 ")

  ;; コンパイル時の文字コード
  (add-hook 'compilation-filter-hook
            '(lambda ()
               ;; シェルモードの入出力文字コード
               (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
               (set-buffer-file-coding-system    'sjis-unix)
               ;; (set-buffer-process-coding-system 'utf-8-dos 'utf-8-unix)
               ;; (set-buffer-file-coding-system    'utf-8-unix)
               ))

  ;; ------------------------------------------------------------------------
  ;; @ menu-tree
  ;; (setq menu-tree-coding-system 'utf-8)
  ;; (require 'menu-tree)

  ;; ------------------------------------------------------------------------
  ;; @ w32-symlinks

  ;; (custom-set-variables '(w32-symlinks-handle-shortcuts t))
  ;; (require 'w32-symlinks)

  ;; (defadvice insert-file-contents-literally
  ;;   (before insert-file-contents-literally-before activate)
  ;;   (set-buffer-multibyte nil))

  ;; (defadvice minibuffer-complete (before expand-symlinks activate)
  ;;   (let ((file (expand-file-name
  ;;                (buffer-substring-no-properties
  ;;                 (line-beginning-position) (line-end-position)))))
  ;;     (when (file-symlink-p file)
  ;;       (delete-region (line-beginning-position) (line-end-position))
  ;;       (insert (w32-symlinks-parse-symlink file)))))

  ;; ------------------------------------------------------------------------
  ;; @ explorer

  ;; カレントディレクトリを explorer (Win) で開く、cygwin 依存
  (defun escape-space-parentheses (str)
    (replace-regexp-in-string " " "\\\\ "
                              (replace-regexp-in-string "(" "\\\\("
                                                        (replace-regexp-in-string ")" "\\\\)" str))))

  (defun explorer (&optional dir)
    (interactive)
    (setq dir (expand-file-name (or dir default-directory)))
    (shell-command (concat "cygstart.exe " (escape-space-parentheses dir))))
  )
 )
