;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-batch "anything")

(require 'anything-startup)

;; ファイル関係のanythingを割り当て
(global-set-key (kbd "C-x b") 'anything-for-files)
;; 前の情報源を表示
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)
;; 次の情報源を表示
(define-key anything-map (kbd "C-M-n") 'anything-next-source)
;; 候補が多いときに体感速度を早くする
(setq anything-quick-update t)
;; 200件まで表示する(デフォルト50件)
(setq anything-candidate-number-limit 200)
;; 候補選択ショートカットをアルファベットに
(setq anything-enable-shortcuts 'alphabet)
;; kill-ring
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

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

;; ;; root権限でアクションを実行するときのコマンド
;; (setq anything-su-or-sudo "sudo")
;; (require 'anything-match-plugin)
;; (and (equal current-language-environment "Japanese")
;;      (executable-find "migemo")
;;      (require 'anything-migemo nil t))
;; (require 'anything-complete)
;; ;; M-xによる補完をAnythingで行う
;; ;; (anything-read-string-mode 1)
(anything-read-string-mode nil)
;; ;; lispシンボルの補完候補の再検索時間
;; (anything-lisp-complete-symbol-set-timer 150)

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
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-files-in-current-dir+
        anything-c-source-locate
        anything-c-source-gtags-select
        )
      "*my-anything*"))
(global-set-key (kbd "C-x C-l") 'my-anything)
(global-set-key (kbd "C-;") 'my-anything)


;; http://d.hatena.ne.jp/mooz/20110320/p1
(require 'cl)  ; loop, delete-duplicates
(defun anything-font-families ()
  "Preconfigured `anything' for font family."
  (interactive)
  (flet ((anything-mp-highlight-match () nil))
    (anything-other-buffer
     '(anything-c-source-font-families)
     "*anything font families*")))
(defun anything-font-families-create-buffer ()
  (with-current-buffer
      (get-buffer-create "*Fonts*")
    (loop for family in (sort (delete-duplicates (font-family-list)) 'string<)
          do (insert
              (propertize (concat family "\n")
                          'font-lock-face
                          (list :family family :height 2.0 :weight 'bold))))
    (font-lock-mode 1)))
(defvar anything-c-source-font-families
  '((name . "Fonts")
    (init lambda ()
          (unless (anything-candidate-buffer)
            (save-window-excursion
              (anything-font-families-create-buffer))
            (anything-candidate-buffer
             (get-buffer "*Fonts*"))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     ("Copy Name" lambda
      (candidate)
      (kill-new candidate))
     ("Insert Name" lambda
      (candidate)
      (with-current-buffer anything-current-buffer
        (insert candidate))))))

;; http://qiita.com/items/2940
(defun anything-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All controlled files in this project (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                 (anything-candidate-buffer))
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format "git ls-files $(git rev-parse --show-cdup) %s"
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))

(defun anything-git-project ()
  (interactive)
  (let* ((pwd (shell-command-to-string "echo -n `pwd`"))
         (sources (anything-c-sources-git-project-for pwd)))
    (anything-other-buffer sources
                           (format "*Anything git project in %s*" pwd))))
;; (define-key global-map (kbd "C-;") 'anything-git-project)

(provide 'init-anything)
