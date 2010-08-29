;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-batch "anything")

;; (require 'anything-startup)
(require 'anything)

(define-key global-map (kbd "C-x C-l") 'anything)
(define-key global-map (kbd "C-;") 'anything)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)
(define-key anything-map (kbd "C-M-n") 'anything-next-source)

;; 候補の最大表示数
(setq anything-candidate-number-limit 100)
;; 候補が多いときに体感速度を早くする
(setq anything-quick-update t)
;; 候補選択ショートカットをアルファベットに
(setq anything-enable-shortcuts 'alphabet)
(require 'anything-config)
;; root権限でアクションを実行するときのコマンド
(setq anything-su-or-sudo "sudo")
(require 'anything-match-plugin)
(and (equal current-language-environment "Japanese")
     (executable-find "migemo")
     (require 'anything-migemo nil t))
(require 'anything-complete)
;; M-xによる補完をAnythingで行う
;; (anything-read-string-mode 1)
;; lispシンボルの補完候補の再検索時間
(anything-lisp-complete-symbol-set-timer 150)

(require 'anything-show-completion)
(require 'anything-auto-install)
(require 'descbinds-anything)
(descbinds-anything-install)
(require 'anything-grep)
(require 'anything-gtags)

;; anythingバッファを最下行に表示
;; (auto-install-from-url "http://nschum.de/src/emacs/split-root/split-root.el")
(require 'split-root)
(defun anything-display-function--split-root (buf)
  (let ((percent 40.0))
    (set-window-buffer
     (split-root-window
      (truncate (* (frame-height) (/ percent 100.0)))) buf)))
(setq anything-display-function 'anything-display-function--split-root)

;; インクリメンタルサーチとoccurを合体
;; (auto-install-from-url "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
(require 'anything-c-moccur)
(setq moccur-split-word t)
(setq anything-c-moccur-anything-idle-delay 0.1)
;; バッファの情報をハイライトする
(setq anything-c-moccur-higligt-info-line-flag t)
;; 現在選択中の候補の位置をほかのwindowに表示する
(setq anything-c-moccur-enable-auto-look-flag t)
;; 起動時にポイントの位置の単語を初期パターンにする
(setq anything-c-moccur-enable-initial-pattern t)
(global-set-key (kbd "M-s") 'anything-c-moccur-occur-by-moccur)
;; インクリメンタルサーチから移行できるように
(define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-from-isearch)
;; 旧来のisearch-moccurはC-M-oへ
(define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)

;; manやinfoを調べるコマンド
(setq anything-for-document-sources
      (list
       anything-c-source-man-pages
       anything-c-source-info-cl
       anything-c-source-info-pages
       anything-c-source-info-elisp
       anything-c-source-apropos-emacs-commands
       anything-c-source-apropos-emacs-functions
       anything-c-source-apropos-emacs-variables))
(defun anything-for-document ()
  "Preconfigured `anything' for anything-for-document."
  (interactive)
  (anything anything-for-document-sources
            (thing-at-point 'symbol) nil nil nil
            "*anything for document*"))

(defun my-anything ()
  (interactive)
  (anything-other-buffer
      '(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers+
        anything-c-source-recentf
        anything-c-source-file-name-history
        anything-c-source-files-in-current-dir+
        ;; anything-c-source-man-pages
        anything-c-source-gtags-select
        anything-c-source-extended-command-history
        anything-c-source-emacs-commands
        anything-c-source-kill-ring
        anything-c-source-bookmarks
        )
      "*my-anything*"))

;; (setq anything-for-files-prefered-list
;;       '(anything-c-source-ffap-line
;;         anything-c-source-ffap-guesser
;;         anything-c-source-buffers+
;;         anything-c-source-recentf
;;         anything-c-source-bookmarks
;;         anything-c-source-file-cache
;;         anything-c-source-files-in-current-dir+
;;         ))

(provide 'init-anything)
