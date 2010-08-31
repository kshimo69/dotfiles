;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://www.emacswiki.org/emacs/TwitteringMode
;; http://sourceforge.net/projects/twmode/files/

(require 'twittering-mode)

(setq twittering-username "kshimo69")
(setq twittering-status-format
      ;; "%C{%Y/%m/%d %H:%M:%S} %s > %T // from %f%L%r%R")
      "%C{%H:%M:%S} %s %T [from %f]%r%R")
(setq twittering-retweet-format "RT: @%s: %t")
;; (setq twittering-reverse-mode t)
(setq twittering-timer-interval 60)

(defun twittering-my-list-timeline ()
  (interactive)
  (twittering-visit-timeline '"kshimo69/jazzout"))

;; 更新はminibufferに入力する
(setq twittering-update-status-function 'twittering-update-status-from-minibuffer)

(add-hook 'twittering-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key twittering-mode-map
                        (read-kbd-macro key) func)))
                  '(("F" . twittering-friends-timeline)
                    ("R" . twittering-replies-timeline)
                    ("U" . twittering-user-timeline)
                    ("J" . twittering-my-list-timeline)
                    ("W" . twittering-update-status-interactive)))))

(defvar twittering-notify-keyword "^.*\\(kshimo69\\|jazzout\\).*$")
(add-hook 'twittering-new-tweets-hook
          (lambda ()
            (dolist (el twittering-new-tweets-statuses)
              (if (string-match twittering-notify-keyword (cdr (assoc 'text el)))
            (start-process "twittering-notify" nil "notify-send"
                           "-i" "/usr/share/pixmaps/redhat/shadowman-transparent-64.png"
                           "Match keyword"
                           (format "%s: %s"
                                   (cdr (assoc 'user-screen-name el))
                                   (cdr (assoc 'text el))
                                   ))))))

(provide 'init-twitter)
