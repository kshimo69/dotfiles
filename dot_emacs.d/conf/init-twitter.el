;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://www.emacswiki.org/emacs/TwitteringMode
;; http://sourceforge.net/projects/twmode/files/

(require 'twittering-mode)

(setq twittering-username "kshimo69")
(setq twittering-status-format
      "%C{%Y/%m/%d %H:%M:%S} %s > %T // from %f%L%r%R")
(setq twittering-retweet-format "RT: @%s: %t")
;; (setq twittering-reverse-mode t)
;; (setq twittering-timer-interval 60)

;; (add-hook 'twittering-new-tweets-hook
;;           (lambda ()
;;             (let ((n twittering-new-tweets-count))
;;               (start-process "twittering-notify" nil "growlnotify"
;;                              "-n" "New Tweet"
;;                              "--image" "/Applications/Emacs.app/Contents/Resources/Emacs.icns"
;;                              "-m"
;;                              (format "You have %d new tweet%s"
;;                                      n (if (> n 1) "s" ""))))))

(provide 'init-twitter)
