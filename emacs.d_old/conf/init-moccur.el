;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; moccur and grep settings.

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
;; ;; lgrepに-0u8オプションをつけると出力がutf-8になる
;; (igrep-define lgrep (igrep-use-zgrep nil)(ignore-regex-option "-n -0u8"))
;; (igrep-find-define lgrep (igrep-use-zgrep nil)(ignore-regex-option "-n -0u8"))
  )

;; 複数のgrepバッファを使う
;; (auto-install-from-emacswiki "grep-a-lot.el")
(when (require 'grep-a-lot)
  (grep-a-lot-setup-keys)
  ;; ;; igrep用
  ;; (grep-a-lot-advise igrep)
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

(provide 'init-moccur)
