;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; スクリプトを実行する
(global-set-key (kbd "C-c p") 'executable-interpret)

;; .、..を展開しない
(setq eshell-glob-include-dot-dot nil)
;; 4m が表示される対策
(setq system-uses-terminfo nil)
;; ls とかの色表示が化ける対策
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; http://sakito.jp/emacs/emacsshell.html
;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      ;; (executable-find "f_zsh") ;; Emacs + Cygwin 用
      ;; (executable-find "f_bash") ;; Emacs + Cygwin 用
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))
;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

;; ansi-termの設定
;; http://d.hatena.ne.jp/mooz/20100405/p1
(defvar my-shell-pop-key (kbd "<f2>"))
(defvar my-ansi-term-toggle-mode-key (kbd "C-t"))

(defadvice ansi-term (after ansi-term-after-advice (arg))
  "run hook as after advice"
  (run-hooks 'ansi-term-after-hook))
(ad-activate 'ansi-term)

;; 実は C-c C-j と C-c C-k で切り換えできる!
(defun my-term-switch-line-char ()
  "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
  (interactive)
  (cond
   ((term-in-line-mode)
    (term-char-mode)
    (hl-line-mode -1))
   ((term-in-char-mode)
    (term-line-mode)
    (hl-line-mode 1))))

(defadvice anything-c-kill-ring-action
  (around my-anything-kill-ring-term-advice activate)
  "In term-mode, use `term-send-raw-string' instead of `insert-for-yank'"
  (if (eq major-mode 'term-mode)
      (letf (((symbol-function 'insert-for-yank)
              (symbol-function 'term-send-raw-string)))
        ad-do-it)
    ad-do-it))

(defvar ansi-term-after-hook nil)
(add-hook 'ansi-term-after-hook
          (lambda ()
            ;; shell-pop
            (define-key term-raw-map my-shell-pop-key 'shell-pop)
            ;; これがないと M-x できなかったり
            (define-key term-raw-map (kbd "M-x") 'nil)
            ;; C-zを奪われるのを防ぐ
            (define-key term-raw-map (kbd "C-z")
              (lookup-key (current-global-map) (kbd "C-z")))
            ;; C-qを奪われるのを防ぐ
            (define-key term-raw-map (kbd "C-q")
              (lookup-key (current-global-map) (kbd "C-q")))
            ;; コピー, 貼り付け
            (define-key term-raw-map (kbd "C-k")
              (lambda (&optional arg) (interactive "P")
                (funcall 'kill-line arg) (term-send-raw)))
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (define-key term-raw-map (kbd "M-y") 'anything-show-kill-ring)
            ;; C-t で line-mode と char-mode を切り替える
            (define-key term-raw-map  my-ansi-term-toggle-mode-key
              'my-term-switch-line-char)
            (define-key term-mode-map my-ansi-term-toggle-mode-key
              'my-term-switch-line-char)
           ))

;; (auto-install-from-emacswiki "shell-pop.el")
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell shell-file-name)
(shell-pop-set-window-height 50)
(global-set-key my-shell-pop-key 'shell-pop)

(provide 'init-shellpop)
