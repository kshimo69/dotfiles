;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\M-g" 'gtags-find-with-grep)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
;; 自動で gtags-mode になるように＆補完リスト作成
(add-hook 'c-mode-common-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))
(add-hook 'python-mode-hook
          '(lambda()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

(provide 'init-gtags)
