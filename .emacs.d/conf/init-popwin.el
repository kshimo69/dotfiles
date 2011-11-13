;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-from-url "https://github.com/m2ym/popwin-el/raw/master/popwin.el")

;; https://github.com/m2ym/popwin-el
(require 'popwin)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)
;; (setq special-display-function 'popwin:special-display-popup-window)
(setq popwin:popup-window-height 0.4)

;; popwinで表示するバッファ
(setq anything-samewindow nil)
(setq popwin:special-display-config
      (append '(
                ;; ("*anything*" :height 20)
                ("*Compile-Log*" :height 20 :noselect t)
                (dired-mode :position top)
                ("*Org Agenda*")
                (" *Agenda Commands*")
                ("*Backtrace*")
                ("*sdic*" :noselect t :height 20)
                (" *auto-async-byte-compile*" :height 20 :noselect t)
                ("*interpretation*")
                ;; ("\\*hg command" :regexp t :noselect)
                ("*grep*\\*" :regexp t)
                (twittering-mode :position top)
                )
              popwin:special-display-config))
(define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)
(define-key dired-mode-map "o" #'(lambda ()
                                   (interactive)
                                   (popwin:find-file
                                    (dired-get-file-for-visit))))

(provide 'init-popwin)
