;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://d.hatena.ne.jp/kobapan/20100126/1264459661
;; (auto-install-from-url "http://www.gentei.org/~yuuji/software/windows.el")
;; (auto-install-from-url "http://www.gentei.org/~yuuji/software/revive.el")
(eval-and-compile (defvar win:switch-prefix "\C-z"))
;; (defvar win:switch-prefix "\C-z")

;; http://d.hatena.ne.jp/peccu/20111207/auto_save_windows_el
;; (require 'anything-c-source-other-windows)
;; ` は「直前の状態」
;; (setq win:base-key ?`)
;; ` 〜 z は27文字
;; (setq win:max-configs 27)
;; C-c英字 に割り当てない
;; (setq win:quick-selection nil)

(require 'windows)
(setq win:use-frame nil)
(win:startup-with-window)
(global-set-key (kbd "C-c C") 'see-you-again)
(global-set-key (kbd "C-c C-r") 'resume-windows)
;; (define-key win:switch-map "=" 'anything-other-windows)
;; (run-with-idle-timer 30 t 'win-save-all-configurations)

;; バッファ全部を自動的に保存・復元
;; http://www.hasta-pronto.org/archives/2008/01/30-0235.php
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe emacs" t)
(global-set-key (kbd "C-x E") 'save-current-configuration) ; C-x E で保存
(global-set-key (kbd "C-x R") 'resume)                     ; C-x R で復元
(global-set-key (kbd "C-x K") 'wipe)                       ; C-x K で Kill
;; (add-hook 'kill-emacs-hook 'save-current-configuration)    ; 終了時に保存

(provide 'init-windows)
