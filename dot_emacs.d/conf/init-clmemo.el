;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; clmemo
(autoload 'clmemo "clmemo" "ChangeLog memo mode." t)
(setq clmemo-file-name "~/clmemo/ChangeLog.txt")
(global-set-key "\C-xM" 'clmemo)
;; タイトルの補完
(setq clmemo-title-list '(
                          ("job"."work")
                          ("kaisha"."work")
                          "work"
                          "task"
                          "todo"
                          "css"
                          "clmemo"
                          "evernote"
                          "linux"
                          "windows"
                          "javascript"
                          "rpm"
                          "psp"
                          "perl"
                          "python"
                          "rtm"
                          "english"
                          "c"
                          "iphone"
                          "mac"
                          "bash"
                          "blosxom"
                          "chalow"
                          "chariot"
                          "cl2blog"
                          "emacs"
                          "vim"
                          "hatena"
                          "hatebu"
                          "music"
                          "book"
                          "sicp"
                          "gtd"
                          "google"
                          "twitter"
                          ))
;; リージョンをメモに追加する
(setq clmemo-buffer-function-list
      '(clmemo-insert-region))
;; サブタイトルをカテゴリ風に
(setq clmemo-subtitle-char "["
      clmemo-subtitle-punctuation-char '( "[" . "]"))
;; エントリヘッダに曜日を追加
(setq clmemo-time-string-with-weekday t)
;; w3m使用時にURLを自動で追加
(setq clmemo-buffer-function-list
      '(clmemo-tag-insert-url-from-w3m))
;; clgrep
(autoload 'clgrep "clgrep" "ChangeLog grep." t)
(autoload 'clgrep-item "clgrep" "ChangeLog grep." t)
(autoload 'clgrep-item-header "clgrep" "ChangeLog grep for item header" t)
(autoload 'clgrep-item-tag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-item-notag "clgrep" "ChangeLog grep for item except for tag" t)
(autoload 'clgrep-item-nourl "clgrep" "ChangeLog grep item except for url" t)
(autoload 'clgrep-entry "clgrep" "ChangeLog grep for entry" t)
(autoload 'clgrep-entry-header "clgrep" "ChangeLog grep for entry header" t)
(autoload 'clgrep-entry-no-entry-header "clgrep" "ChangeLog grep for entry except entry header" t)
(autoload 'clgrep-entry-tag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-entry-notag "clgrep" "ChangeLog grep for tag" t)
(autoload 'clgrep-entry-nourl "clgrep" "ChangeLog grep entry except for url" t)
(add-hook 'clmemo-mode-hook
          '(lambda () (define-key clmemo-mode-map "\C-c\C-g" 'clgrep)))

;; item-headerに空白があっても色をつける
(defface my-clmemo-item-header-face
  '((
     ((class color) (background light))
    ;;  (:background "white" :foreground "skyblue"))
    ;;  (:background "black" :foreground "skyblue"))
     (:foreground "skyblue"))
    (
     ((class color) (background dark))
     ;; (:background "black" :foreground "skyblue"))
     (:foreground "skyblue"))
    (t
     (:bold t)))
  "Face for highlighting item header.")
(add-hook 'change-log-mode-hook
          '(lambda()
             (highlight-regexp "\\* *.+:" "my-clmemo-item-header-face")))

(provide 'init-clmemo)
