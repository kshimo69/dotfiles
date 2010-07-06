;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://www.emacswiki.org/emacs/TwitteringMode

(require 'twittering-mode)
(setq twittering-username "kshimo69")

(add-hook 'twittering-mode-hook
          (lambda ()
            (local-set-key "f" 'twittering-friends-timeline)
            (local-set-key "r" 'twittering-replies-timeline)
            (local-set-key "u" 'twittering-user-timeline)
            (local-set-key "w" 'twittering-update-status-interactive)))

(twittering-icon-mode)
(setq twittering-timer-interval 120)

(add-hook 'twittering-new-tweets-hook
          (lambda ()
            (let ((n twittering-new-tweets-count))
              (start-process "twittering-notify" nil "growlnotify"
                             "-n" "New Tweet"
                             "--image" "/Applications/Emacs.app/Contents/Resources/Emacs.icns"
                             "-m"
                             (format "You have %d new tweet%s"
                                     n (if (> n 1) "s" ""))))))

(provide 'init-twitter)
