;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

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

(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell shell-file-name)
(shell-pop-set-window-height 30)
(global-set-key [f2] 'shell-pop)

(provide 'init-shellpop)
