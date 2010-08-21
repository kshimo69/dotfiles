;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; (auto-install-batch "anything")

(require 'anything)
(require 'anything-config)
(require 'anything-gtags)

(define-key global-map (kbd "C-x C-l") 'anything)
(define-key global-map (kbd "C-;") 'anything)

(setq anything-sources
      '(
        anything-c-source-ffap-line
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
        ))

(setq anything-for-files-prefered-list
      '(anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-buffers+
        anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-files-in-current-dir+
        ))

(provide 'init-anything)
