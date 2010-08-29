;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

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

(setq user-full-name "Kimihiko Shimomura")
(setq user-mail-address "kshimo69@gmail.com")

;; 初期位置
(cd "~/")

;; exec-path、PATH、MANPATHの追加 (下が優先)
;; http://sakito.jp/emacs/emacsshell.html
(dolist (dir (list
              (expand-file-name "~/bin")
              "/sbin"
              "/bin"
              "/usr/sbin"
              "/usr/bin"
              "/usr/local/sbin"
              "/usr/local/bin"
              "/opt/local/sbin"
              "/opt/local/bin"
              (expand-file-name "~/local/bin")
              ))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))
(setenv "MANPATH" (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man" (getenv "MANPATH")))

;; スタートアップ時のメッセージを抑制
(setq inhibit-startup-message t)
;; scratch のメッセージを空にする
(setq initial-scratch-message nil)

;; デバッグモード
(setq debug-on-error t)

;; emacs-serverを起動(emacsclientで使用)
(require 'server)
(unless (server-running-p) (server-start))
(setq server-visit-hook
      '(lambda ()
         ;; Frame を前面にする
         (raise-frame (selected-frame))
         ;; キーボードフォーカスを選択しているFrameにする
         (x-focus-frame (selected-frame))))

;; 文字コード
(set-language-environment 'Japanese)
;; (set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)
(cond
 (mac-p
  ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱う
  ;; 以下はファイル名を NFC で扱う環境と共同作業等する場合の対処
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 (windows-p
  (setq file-name-coding-system 'sjis)
  (setq locale-coding-system 'utf-8))
 (t
  (setq file-name-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)))

;; メニューバー、ツールバー非表示
;; (menu-bar-mode nil)
(tool-bar-mode nil)
(scroll-bar-mode nil)

;; ヴィジブルベルを抑制
(setq visible-bell nil)

;; ビープ音を抑制
(setq ring-bell-function '(lambda ()))

;; カーソルの点滅を抑制
;; (blink-cursor-mode 0)

;; 行数、列数を表示
(line-number-mode t)
(column-number-mode t)

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
        ("." . "~/.emacs.d/var/emacs")
        ))

;; 使わないバッファを自動的に消す
;; (auto-install-from-emacswiki "tempbuf.el")
(require 'tempbuf)
;; ファイルを開いたら自動的にtempbufを有効にする
(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; diredバッファに対してtempbufを有効にする
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)

;; ファイル保存時に自動的にバイトコンパイルする
;; (auto-install-from-emacswiki "auto-async-byte-compile.el")
(require 'auto-async-byte-compile)
;; 自動バイトコンパイルを無効にするファイル名の正規表現
(setq auto-async-byte-compile-exclude-files-regexp "/hoge")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; TAB はスペース 4 個ぶんを基本
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; 個人用infoディレクトリを追加
(setq Info-default-directory-list
      (cons (expand-file-name "~/.emacs.d/info/") Info-default-directory-list))

;; バッファ全部を自動的に保存・復元
;; http://www.hasta-pronto.org/archives/2008/01/30-0235.php
;; (auto-install-from-url "http://www.gentei.org/~yuuji/software/revive.el")
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe emacs" t)
(global-set-key (kbd "C-x E") 'save-current-configuration) ; C-x E で保存
(global-set-key (kbd "C-x R") 'resume)                     ; C-x R で復元
(global-set-key (kbd "C-x K") 'wipe)                       ; C-x K で Kill
(add-hook 'kill-emacs-hook 'save-current-configuration)    ; 終了時に保存

;; ファイル名の1階層上を表示する
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;; *で囲まれたバッファ名は対象外
  (setq uniquify-ignore-buffers-re "*[^*]+*")
  )

;; シンボリックファイルを開く時にいちいち聞かない
(setq vc-follow-symlinks t)

;; Scroll buffer without moving the cursor
(global-set-key (kbd "M-p") '(lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") '(lambda () (interactive) (scroll-up 1)))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; default to unified diffs
(setq diff-switches "-u")

;; 時刻を表示
(setq display-time-string-forms
      '(24-hours ":" minutes " " month "/" day "(" dayname ")"))
(display-time)

;; 曜日表示は英語
(setq system-time-locale "C")

;; ファイルの最後には \n
(setq require-final-newline t)

;; BSで選択範囲を消す
(delete-selection-mode 1)

;; 履歴一覧を開く
(global-set-key (kbd "C-x f") 'recentf-open-files)
;; (auto-install-from-emacswiki "recentf-ext.el")
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("/GTAGS$"))
(require 'recentf-ext)

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

;; カーソル位置を戻す
;; (auto-install-from-emacswiki "point-undo.el")
(require 'point-undo)
(global-set-key (kbd "<f6>") 'point-undo)
(global-set-key (kbd "S-<f6>") 'point-redo)

;; 変更箇所にジャンプする
;; (auto-install-from-emacswiki "goto-chg.el")
(require 'goto-chg)
(global-set-key (kbd "<f7>") 'goto-last-change)
(global-set-key (kbd "S-<f7>") 'goto-last-change-reverse)

;; buffer切り替えを使い易く
(iswitchb-mode 1)
;; C-f, C-b, C-n, C-p で候補を切り替えることができるように。
(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (define-key iswitchb-mode-map (kbd "C-n") 'iswitchb-next-match)
            (define-key iswitchb-mode-map (kbd "C-p") 'iswitchb-prev-match)
            (define-key iswitchb-mode-map (kbd "C-f") 'iswitchb-next-match)
            (define-key iswitchb-mode-map (kbd "C-b") 'iswitchb-prev-match)
            ))
(global-set-key (kbd "C-x C-b") 'bs-show)

;; ウインドウリサイズを簡単に
;; http://d.hatena.ne.jp/mooz/20100119/p1
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        c)
    (catch 'end-flag
      (while t
        (message "size[%dx%d]"
                 (window-width) (window-height))
        (setq c (read-char))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (message "Quit")
               (throw 'end-flag t)))))))
;; (global-set-key (kbd "C-c C-r") 'window-resizer)

;; C-q をプリフィックスキー化
(global-set-key (kbd "C-q") (make-sparse-keymap))

;; quoted-insert は C-q C-q へ割り当て
(global-set-key (kbd "C-q C-q") 'quoted-insert)

;; window-resizer は C-q C-r (resize) で
(global-set-key (kbd "C-q C-r") 'window-resizer)

;; C-x o にはもううんざり
(global-set-key (kbd "C-q l") 'windmove-right)
(global-set-key (kbd "C-q h") 'windmove-left)
(global-set-key (kbd "C-q j") 'windmove-down)
(global-set-key (kbd "C-q k") 'windmove-up)

;; C-hをバックスペースに
(global-set-key (kbd "C-h") 'delete-backward-char)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(global-set-key (kbd "C-x C-h") 'help-command)

;; カーソル位置の単語をkill-ringにコピー
(ffap-bindings)                         ; カーソル位置のファイル名、URLで開く
(global-set-key (kbd "M-c") 'ffap-copy-string-as-kill)

;; kill-ringに同じ内容の文字列を入れない
;; http://d.hatena.ne.jp/kitokitoki/20100515/p1
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; C-zでウインドウ分割 or 移動
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-z") 'other-window-or-split)

;; カーソル位置の単語でgrep-find
(defun grep-find-current-word ()
  (interactive)
  (let ((command "find . -type f -print0 | xargs -0 grep -n -i "))
    (ffap-copy-string-as-kill)
    (grep-find (format "%s %s"
                       command (car kill-ring)))))
(global-set-key (kbd "C-c g") 'grep-find-current-word)

;; igrep - 対話的にgrepする
;; (auto-install-from-emacswiki "igrep.el")
(when (require 'igrep)
  ;; lgrepに-0u8オプションをつけると出力がutf-8になる
  (igrep-define lgrep (igrep-use-zgrep nil)(ignore-regex-option "-n -0u8"))
  (igrep-find-define lgrep (igrep-use-zgrep nil)(ignore-regex-option "-n -0u8"))
  )

;; 複数のgrepバッファを使う
;; (auto-install-from-emacswiki "grep-a-lot.el")
(when (require 'grep-a-lot)
  (grep-a-lot-setup-keys)
  ;; igrep用
  (grep-a-lot-advise igrep)
  )

;; grepの検索結果を編集する
;; (auto-install-from-emacswiki "grep-edit.el")
(when (require 'grep-edit))

;; color-moccur
;; (auto-install-from-emacswiki "color-moccur.el")
;; (auto-install-from-emacswiki "moccur-edit.el")
(when (require 'color-moccur)
  ;; M-oをoccur-by-moccurに
  (global-set-key (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索で除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (require 'moccur-edit nil t)
  ;; MigemoがあればMigemoを使う
  (when (and (executable-find "migemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t)))

;; 範囲指定してない時にC-wで前の単語を削除
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))
;; minibuffer用
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;; Emacsを終了してもファイルを編集してた位置やminibuffer への入力内容を覚えておく
(when (require 'session nil t)
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
;; ファイル内のカーソル位置を記録する
(setq-default save-place t)
(require 'saveplace)
;; ログの記録行数を減らす
(setq message-log-max 10000)
;; ミニバッファを再帰的に呼び出せるようにする
(setq enable-recursive-minibuffers t)

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

;; fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    ))
(global-set-key (kbd "C-c m") 'toggle-fullscreen)

;; 起動時に縦分割
(setq w (selected-window))
(split-window w nil t)

;; C-+とC--でフォントサイズを変える
;; http://emacs-fu.blogspot.com/2008/12/zooming-inout.html
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height)
                         (* (if (> n 0) 1 -1) 10))))
(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))

;; コマンド連続実行時の動作を変える
;; (auto-install-batch "sequential-command")
(require 'sequential-command-config)
(sequential-command-setup-keys)

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

;; 括弧の対応を取りながらS式を編集する
;; (auto-install-from-url "http://mumble.net/~campbell/emacs/paredit.el")
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; Emacs Lisp関数・変数のヘルプをエコーエリアに表示する
;; (auto-install-from-emacswiki "eldoc-extension.el")
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;; Emacs Lisp式の値をコメントで注釈する
;; (auto-install-from-emacswiki "lispxmp.el")
(require 'lispxmp)

;; ユニットテストを書く
;; (auto-install-batch "el-expectations")
(require 'el-expectations)

(provide 'init-global)
