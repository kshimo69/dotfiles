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
;;
;; 変数の意味を調べる
;; M-x apropos-variable
;; 変数の値を調べる
;; M-x describe-variable

;; 常時デバッグ状態
(setq debug-on-error t)

;; Always load newest byte code
(setq load-prefer-newer t)

;; local設定があったら読む
;; proxy設定とかあったら先に読みたいのでここ
(when (locate-library "passwd")
  (require 'passwd))

;; packageの設定
(require 'package)
(setq package-enable-at-startup nil)
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; custom.elを分離
;; load-pathが通ってから読むのでここ
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(progn ; `use-package'
  ;;(setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (setq use-package-always-ensure t)
  (require 'use-package))
(use-package diminish)
(use-package bind-key)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(progn ; `startup'
  (setq user-full-name "Kimihiko Shimomura")
  (setq user-mail-address "kshimo69@gmail.com")
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (fset 'yes-or-no-p 'y-or-n-p))

(progn ; `font'
  (set-language-environment  'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-face-attribute 'default nil
                      :family "Cica"
                      :height 140))

(progn ; `files'
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  (setq confirm-kill-emacs 'y-or-n-p)
  (put 'set-goal-column 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (setq vc-follow-symlinks t)
  (setq diff-switches "-u")
  (setq require-final-newline t)
  (delete-selection-mode t)
  ;; C-u C-SPC C-SPC... でカーソル位置を辿る
  (setq set-mark-command-repeat-pop t)
  ;; カーソル位置のファイル名、URLで開く
  (ffap-bindings)
  ;; 行末の空白をめだたせる M-x delete-trailing-whitespaceで削除出来る
  (when (boundp 'show-trailing-whitespace) (setq-default show-trailing-whitespace t))
  ;;(setq make-backup-files nil)
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
  (setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-"))

(progn ; `window'
  (menu-bar-mode +1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq visible-bell nil)
  (setq ring-bell-function '(lambda ()))
  (global-linum-mode -1)
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
  (setq system-time-locale "C"))

(progn ; `browser'
  (setq eww-search-prefix "https://www.google.co.jp/search?btnI&q="))

;; テーマ用フォント
(use-package all-the-icons)
;; M-x all-the-icons-install-fonts

;; テーマ
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  ;; (doom-modeline-major-mode-icon nil)
  ;; (doom-modeline-minor-modes nil)
  :hook
  (after-init . doom-modeline-mode)
  ;; :config
  ;; (line-number-mode 0)
  ;; (column-number-mode 0)
  ;; (doom-modeline-def-modeline 'main
  ;;   ;; '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
  ;;   '(bar xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
  ;;   '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)
  ;;   )
  ;; (setq doom-modeline-minor-modes t)
  )

(global-hl-line-mode +1)

;; magit
(use-package magit)
(use-package gist
  :config
  (setq gist-view-gist t)
  )
(use-package git-gutter-fringe+
  :config
  (global-git-gutter+-mode t)
  )

(use-package alert)

;; org-mode
(use-package org
  :pin "org"
  :defer t
  :mode (("\\.org\\'" . org-mode))
  :hook (org-mode-hook . git-gutter+-disabled-modes)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  (:map org-mode-map
        ("C-c C-p" . outline-previous-visible-heading)
        ("C-c C-n" . outline-next-visible-heading))
  :config
  (setq org-directory "~/org/"
        org-default-notes-file (concat org-directory "agenda.org")
        org-agenda-files '("~/org/agenda.org"
                           ;; "~/org/code-reading.org"
                           ;; "~/org/mobileorg.org"
                           )
        org-log-done 'time
        org-startup-truncated nil
        org-use-speed-commands t
        org-enforce-todo-dependencies t
        org-return-follows-link t
        org-agenda-start-with-clockreport-mode t
        org-src-fontify-natively t)
  (setq org-agenda-custom-commands
        '(("x" "Unscheduled TODO" tags-todo "-SCHEDULED=>\"<now>\"" nil)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCEL(c!)")
          (sequence "NEW(n)" "ASSIGNED(a!)" "|" "FIXED(f!)")
          ))
  (use-package org-journal
    :commands (org-journal-new-entry)
    :custom
    (org-journal-dir "~/org/journal")
    (org-journal-date-format "%A, %d %B %Y"))
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (goto-char (point-min)))
  (setq org-capture-templates
        '(("n" "Note" entry
           (file+headline org-default-notes-file "Note")
           "** %?\n   Added: %T\n   %a\   %i\n")
          ("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "** TODO %?\n   Added: %T\n   %a\n   %i\n")
          ;; ("t" "Todo" entry
          ;;  (file+headline (lambda () (concat (file-name-directory buffer-file-name) "agenda.org")) "Tasks")
          ;;  "** TODO %?\n   Added: %T\n   %a\n   %i\n")
          ("b" "Bug" entry
           (file+headline org-default-notes-file "Tasks")
           "** NEW %?   :bug:\n   Added: %T\n   %a\n   %i\n")
          ("i" "Idea" entry
           (file+headline org-default-notes-file "New Ideas")
           "** SOMEDAY %?\n   Added: %T\n   %a\   %i\n")
          ("d" "Daily review" entry
           (file+headline org-default-notes-file "Tasks")
           "** TODO Daily Review[/] :review:\n%?   DEADLINE: %t\n%[~/org/daily_review.txt]")
          ("w" "Weekly review" entry
           (file+headline org-default-notes-file "Tasks")
           "** TODO Weekly Review %T[/] :review:\n%?%[~/org/weekly_review.txt]")
          ("j" "Journal" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
          ))
  (setq org-agenda-current-time-string "← now")
  (setq org-agenda-time-grid
        (quote
         ((daily today remove-match)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
          "-" "────────────────"
          )))
  )
;; (use-package org-pomodoro
;;     :after org-agenda
;;     :custom
;;     (org-pomodoro-ask-upon-killing t)
;;     (org-pomodoro-format "%s")
;;     (org-pomodoro-short-break-format "%s")
;;     (org-pomodoro-long-break-format  "%s")
;;     :custom-face
;;     (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
;;     (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
;;     :hook
;;     (org-pomodoro-started . (lambda () (notifications-notify
;;                                                :title "org-pomodoro"
;;                            :body "Let's focus for 25 minutes!"
;;                            :app-icon "~/.emacs.d/img/001-food-and-restaurant.png")))
;;     (org-pomodoro-finished . (lambda () (notifications-notify
;;                                                :title "org-pomodoro"
;;                            :body "Well done! Take a break."
;;                            :app-icon "~/.emacs.d/img/004-beer.png")))
;;     :config
;;     :bind (:map org-agenda-mode-map
;;                 ("p" . org-pomodoro)))

;; server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; ファイル名の1階層上を表示する
(use-package uniquify
  :ensure nil
  :init
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    ;; *で囲まれたバッファ名は対象外
    (setq uniquify-ignore-buffers-re "*[^*]+*")
    )
  )

(use-package popwin
  :init
  (setq popwin:popup-window-position 'bottom)
  (setq popwin:special-display-config
        '(
          ("*Warnings*" :regexp t)
          ("*Org Agenda*")
          ("*Help*")
          )
        )
  :config
  (popwin-mode +1)
  )

(use-package neotree
  :config
  (setq neo-show-hidden-files t)
  (setq neo-persist-show t)
  ;; (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t))))
  :bind (("C-x C-t" . neotree-toggle))
  )

(use-package paren
  :config
  (show-paren-mode +1)
  (setq show-paren-style 'mixed)
  )

(use-package helm
  ;; :init
  ;; (setq helm-ff-file-name-history-use-recentf t)
  ;; (setq helm-display-function #'display-buffer)
  :bind
  (("M-x" . 'helm-M-x)
   ("C-;" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("C-c o"   . helm-occur)
   :map helm-map
   ("C-h" . delete-backward-char)
   ("C-w" . backward-kill-word)
   :map helm-find-files-map
   ("C-h" . delete-backward-char)
   ("C-w" . backward-kill-word)
   ("TAB" . helm-execute-persistent-action)
   :map helm-read-file-map
   ("TAB" . helm-execute-persistent-action))
  :config
  (helm-mode 1)
  )

(use-package company
  :defer nil
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("TAB" . company-complete-selection)
        ("C-s" . company-filter-candidates))
  (:map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq x-select-enable-clipboard t)
  (setq save-interprogram-paste-before-kill t)
  (setq yank-pop-change-selection t)
  (setq x-select-enable-clipboard-manager t)
  (setq x-select-enable-primary t)
  (setq mouse-drag-copy-region t)
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (add-to-list 'exec-path-from-shell-variables "http_proxy")
  (add-to-list 'exec-path-from-shell-variables "https_proxy")
  (add-to-list 'exec-path-from-shell-variables "no_proxy")
  (exec-path-from-shell-initialize)
  )

(use-package ripgrep
  :bind
  (
   ("C-c g" . ripgrep-regexp)
   )
  :config
  (setq ripgrep-arguments '("-S"))
  )

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  )

(use-package amx)

;; kill-ringに同じ内容の文字列を入れない
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

(setq history-delete-duplicates t)

;; Emacsを終了してもファイルを編集してた位置やminibuffer への入力内容を覚えておく
(use-package session
  :hook (after-init . session-initialize)
  :config
  (setq session-save-file (expand-file-name "~/.emacs.d/var/session"))
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  ;; これがないと file-name-history に500個保存する前に max-string に達する
  (setq session-globals-max-string 100000000)
  ;; デフォルトでは30!
  (setq history-length t)
  )

;; 履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;; (setq savehist-file (expand-file-name "~/.emacs.d/var/history"))
(setq savehist-file "~/.emacs.d/var/history")
;; savehistのファイルに保存する履歴からfile-name-historyをのぞく
(setq savehist-ignored-variables '(file-name-history))

;; ログの記録行数を減らす
(setq message-log-max 10000)

;; *scratch* バッファを消さないように
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))
(use-package auto-save-buffers-enhanced
  :init
  (auto-save-buffers-enhanced t)
  :config
  (setq auto-save-buffers-enhanced-interval 3600)
  (setq auto-save-buffers-enhanced-exclude-regexps '(".+"))
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer (expand-file-name "~/.emacs.d/var/scratch-backup.el"))
  )

(use-package open-junk-file
  :commands (open-junk-file)
  :config
  (setq open-junk-file-format "~/junk/%Y%m%d-%H%M%S."))

(use-package yaml-mode
  :mode ("\\.yml\\'"
         "\\.yaml\\'")
  )

;;;
;;; key-bind
;;;

;; Mac用設定
(when (memq window-system '(mac ns))
  (setq grep-find-use-xargs 'bsd)
  (setq browse-url-generic-program "open")
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super))
  ;; Ctrl/Cmd/Optionがシステムに渡されるのを防ぐ
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-option-to-system nil)
  )

;; C-hをバックスペースに
(global-set-key (kbd "C-h") 'delete-backward-char)
;; mini-bufferとかどこでも効くように
;; (keyboard-translate ?\C-h ?\C-?)

;; C-h に割り当てられている関数 help-command を C-x C-h に割り当てる
(global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "<f1>") 'help-for-help)

;; Scroll buffer without moving the cursor
(global-set-key (kbd "M-p") '(lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-n") '(lambda () (interactive) (scroll-up 1)))

;; C-t をプリフィックスキー化
(global-set-key (kbd "C-t") (make-sparse-keymap))

(global-set-key (kbd "C-t C-r") 'window-resizer)
(global-set-key (kbd "C-t l") 'windmove-right)
(global-set-key (kbd "C-t h") 'windmove-left)
(global-set-key (kbd "C-t j") 'windmove-down)
(global-set-key (kbd "C-t k") 'windmove-up)

;; C-t C-tでウインドウ分割 or 移動
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "C-t C-t") 'other-window-or-split)

;; カーソル位置の単語をコピー
(global-set-key (kbd "M-c") 'ffap-copy-string-as-kill)

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
