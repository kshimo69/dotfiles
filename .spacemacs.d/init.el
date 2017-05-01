;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     yaml
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     osx
     helm
     ;; better-defaults
     emacs-lisp
     git
     markdown
     (org :variables
          org-enable-github-support t
      )
     pandoc
     deft
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     ;; (spell-checking :variables
     ;;                 enable-flyspell-auto-completion t)
     syntax-checking
     version-control
     themes-megapack
     (elfeed :variables
             rmh-elfeed-org-files (list "~/org/elfeed.org")
             ;; elfeed-enable-web-interface t
      )
     html
     python
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     ;; plantuml
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      )
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     key-chord
     ddskk
     ;; quickrun
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7)
                                (agenda . 5)
                                (todos . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         monokai)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Ricty Diminished Discord"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 95
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 70
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; proxy configuration
  ;; (setq url-proxy-services
  ;;     '(("http" . "proxy.example.com:8080")
  ;;       ("https" . "proxy.example.com:8080")
  ;;       ("no_proxy" . "^\\(localhost\\|127\\.0\\.0\\.1\\|10.*\\)")))
  (setq proxy-config-file "~/.spacemacs.d/proxy-config.el")
  (cond
   ((file-exists-p proxy-config-file)
    (load proxy-config-file)))

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Ctrl-hをバックスペースに
  (global-set-key (kbd "C-h") 'delete-backward-char)

  ;; commandキーをaltに、optionをsuperに
  (setq mac-option-modifier 'super)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)

  ;; insert modeをjjで抜ける
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jj" 'evil-escape)
  (key-chord-define evil-hybrid-state-map "jj" 'evil-escape)
  (key-chord-mode 1)

  ;; デフォルトのインデントは4スペース
  (setq-default tab-width 4
                indent-tabs-mode nil
                )

  ;; 右から左に読む言語に対応させないことで描画高速化
  (setq-default bidi-display-reordering nil)

  ;; 同じ内容を履歴に記録しないようにする
  (setq history-delete-duplicates t)

  ;; モードラインに時刻を表示する
  (setq display-time-string-forms
        ;; '(24-hours ":" minutes " " month "/" day "(" dayname ")"))
        '(24-hours ":" minutes))
  (display-time)

  ;; deft
  (setq deft-directory "~/memo"
        deft-extensions '("org" "md" "txt")
        deft-use-filename-as-title t
        deft-text-mode 'org-mode
        )

  ;; org-mode
  (with-eval-after-load 'org
    (setq org-directory "~/org/")
    ;; (setq org-agenda-files '("~/org/"))
    (setq org-agenda-files (list "~/org/agenda.org"))
    (setq org-default-notes-file (concat org-directory "agenda.org"))
    (setq org-archive-location (concat "%s_archive_"
                                       (format-time-string "%Y" (current-time))
                                       "::* Archived Tasks"
                                       ))
    ;; バッファ内のコードブロッックをそのコード用のモードと同じ色でハイライト
    (setq org-src-fontify-natively t)
    ;; カーソルをコードに合わせて C-c ' するとそのモードで編集バッファが立ちあがる

    ;; TODO keywords as workflow state
    (setq org-todo-keywords
          '((sequence "TODO(t)" "SOMEDAY(s)" "REFERENCE(r)" "WAIT(w)" "|" "DONE(d!)" "CANCEL(c!)")
            ))
    ;; record DONE time
    (setq org-log-done 'time)

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
            ("d" "Daily review" entry
             (file+headline nil "Tasks")
             "** TODO Daily Review[/] :review:\n   Added: %T\n   DEADLINE: %t\n%?%[~/org/daily_review.txt]")
            ("w" "Weekly review" entry
             (file+headline nil "Tasks")
             "** TODO Weekly Review %T[/] :review:\n%?%[~/org/weekly_review.txt]")
            ))

    ;; モードラインにorg-modeの時計を表示する
    (setq spaceline-org-clock-p t)

    )

  ;; eww
  (with-eval-after-load 'eww
    (define-key eww-mode-map "r" 'eww-reload)
    (define-key eww-mode-map "c" 'eww-copy-page-url)
    (define-key eww-mode-map "p" 'scroll-down)
    (define-key eww-mode-map "n" 'scroll-up)

    ;; 背景色を消す
    (defvar eww-disable-colorize t)
    (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
      (unless eww-disable-colorize
        (funcall orig start end fg)))
    (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
    (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
    (defun eww-disable-color ()
      "eww で文字色を反映させない"
      (interactive)
      (setq-local eww-disable-colorize t)
      (eww-reload))
    (defun eww-enable-color ()
      "eww で文字色を反映させる"
      (interactive)
      (setq-local eww-disable-colorize nil)
      (eww-reload))

    ;; デフォルト検索エンジンはgoogle
    (setq eww-search-prefix "http://www.google.co.jp/search?q=")
    )

  ;; Private settings
  ;; (setq private-settings-file "~/.spacemacs.d/private.el")
  ;; (cond
  ;;  ((file-exists-p private-settings-file)
  ;;   (load private-settings-file)))

  ;; bind mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  ;; js2-mode
  ;; (add-hook 'js2-mode-hook
  ;;           (lambda ()
  ;;             (c-set-offset 'case-label' 4)
  ;;             (c-set-offset 'arglist-intro' 4)
  ;;             (c-set-offset 'arglist-cont-nonempty' 4)
  ;;             (c-set-offset 'arglist-close' 0)
  ;;             (setq indent-tabs-mode t)
  ;;             ))

  (add-hook 'c++-mode-hook
            (lambda ()
              (c-set-style "bsd")
              (setq indent-tabs-mode nil)  ;インデントはスペース
              (setq tab-width 4)           ;タブ幅は4
              (setq c-auto-newline nil)    ;全自動インデントは無効
              (c-toggle-hungry-state)      ;BSでいい感じに消してくれる
              (setq c-basic-offset 4)      ;オフセットは4つ
              ;; C-c C-sでインデントに効いてる変数を見つける
              (c-set-offset 'innamespace 0)     ;namespace{}の中はインデントしない
              (c-set-offset 'namespace-open 0)  ;namespaceの開き中括弧
              (c-set-offset 'namespace-close 0) ;namespaceの閉じ中括弧
              (c-set-offset 'defun-open 0)      ;関数定義開始の中括弧
              (c-set-offset 'defun-close 0)     ;関数定義終了の中括弧
              (c-set-offset 'defun-block-intro '+) ;関数内ブロック
              (c-set-offset 'else-clause 0)       ;if-elseのelse
              (c-set-offset 'extern-lang-open 0)  ;externの開始中括弧
              (c-set-offset 'extern-lang-close 0) ;externの終了中括弧
              (c-set-offset 'inextern-lang '+)    ;extern内の要素
              (c-set-offset 'friend 0)       ;friend宣言
              (c-set-offset 'inclass '+)     ;class定義内の要素
              (c-set-offset 'inline-open 0)  ;class内のinline methodの開き中括弧
              (c-set-offset 'inline-close 0) ;class内のinline methodの閉じ中括弧
              (c-set-offset 'label 0)        ;ラベル
              (c-set-offset 'member-init-intro '+) ;member初期化リストの1行目
              (c-set-offset 'member-init-cont '+)  ;member初期化リストの2行目以降
              (c-set-offset 'statement 0)          ;通常の文
              (c-set-offset 'statement-block-intro '+) ;新規文ブロックの1行目
              (c-set-offset 'statement-case-intro '+)  ;caseブロックの1行目
              (c-set-offset 'statement-case-open 0)    ;case文の開き中括弧
              (c-set-offset 'statement-cout '+) ;文の継続する行
              (c-set-offset 'stream-op '+)      ;<<演算子が続く行の2行目以降
              (c-set-offset 'string 0)          ;複数行に跨るliteralの内側
              (c-set-offset 'substatement '+)   ;if,while,forとかの1行目
              (c-set-offset 'substatement-open 0)  ;部分文の開き中括弧
              (c-set-offset 'topmost-intro 0)      ;最上位の言語構成要素の1行目
              (c-set-offset 'topmost-intro-cout 0) ;最上位の言語構成要素の2行目以降
              ))

  ;; whitespaceの設定
  ;; (setq whitespace-style
  ;;       '(
  ;;         face           ; faceで可視化
  ;;         trailing       ; 行末
  ;;         tabs           ; タブ
  ;;         ;; spaces         ; スペース
  ;;         space-mark     ; 表示のマッピング
  ;;         tab-mark
  ;;         ))
  ;; (setq whitespace-display-mappings
  ;;       '(
  ;;         (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
  ;;         ))
  ;; (global-whitespace-mode t)

  ;; skkの設定
  (when (require 'skk nil t)
    (global-set-key (kbd "C-x j") 'skk-auto-fill-mode)
    (setq default-input-method "japanese-skk")
    (require 'skk-study)

    ;; 英字モードの時の色を通常時の色に
    (setq skk-cursor-latin-color skk-cursor-default-color)

    ;; 辞書
    (setq skk-large-jisyo "~/.skk/SKK-JISYO.L")  ; 一応
    ;; use skkserve
    ;; (setq skk-server-host "localhost")
    ;; (setq skk-server-portnum 1178)
    (setq skk-share-private-jisyo t)  ; 複数 skk 辞書を共有

    ;; ノーマルステート時に状態遷移した時に、skkが起動している場合、自動的にアスキーモードにする
    (when (locate-library "skk")
      (require 'skk)
      (defun my-skk-control ()
        (when skk-mode
          (skk-latin-mode 1)))
      (add-hook 'evil-normal-state-entry-hook 'my-skk-control))

    ;; ミニバッファでは C-j を改行にしない
    (define-key minibuffer-local-map (kbd "C-j") 'skk-kakutei)

    ;; ";"をsticky shiftに用いることにする
    (setq skk-sticky-key ";")

    ;; 候補表示
    ;; (setq skk-show-inline t)   ; 変換候補の表示位置
    ;; (setq skk-show-tooltip t) ; 変換候補の表示位置
    ;; (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
    ;; (setq skk-henkan-show-candidates-rows 2) ; 候補表示件数を2列に

    ;; 動的候補表示
    (setq skk-dcomp-activate t) ; 動的補完
    (setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
    (setq skk-dcomp-multiple-rows 10) ; 動的補完の候補表示件数
    )

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-Y-yank-to-eol t)
 '(org-agenda-time-grid
   (quote
    ((daily today)
     "----------------"
     (800 1000 1200 1400 1600 1800 2000 2200))))
 '(package-selected-packages
   (quote
    (ddskk cdb ccc winum solarized-theme madhat2r-theme fuzzy disaster company-c-headers cmake-mode clang-format yaml-mode key-chord yapfify xterm-color web-mode tagedit slim-mode shell-pop scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements multi-term live-py-mode less-css-mode insert-shebang hy-mode helm-pydoc helm-css-scss haml-mode git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter flyspell-popup flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck fish-mode eshell-z eshell-prompt-extras esh-help emmet-mode diff-hl cython-mode company-web web-completion-data company-shell company-anaconda auto-dictionary anaconda-mode pythonic company-quickhelp pos-tip ox-gfm elfeed-web simple-httpd elfeed-org elfeed-goodies ace-jump-mode noflet elfeed pandoc-mode ox-pandoc ht deft zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme org-projectile org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl smeargle orgit org mmm-mode markdown-toc markdown-mode magit-gitflow helm-gitignore helm-company helm-c-yasnippet gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md evil-magit magit magit-popup git-commit with-editor company-statistics company auto-yasnippet yasnippet ac-ispell auto-complete ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
