;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'html-helper-mode)
(require 'simple-hatena-mode)
(require 'hatenahelper-mode)
(setq simple-hatena-default-id "kshimo69")
(setq simple-hatena-bin "~/bin/hw.pl")
(setq simple-hatena-time-offset 6)
(add-hook 'simple-hatena-mode-hook
          '(lambda ()
             (hatenahelper-mode 1)))

(provide 'init-hatena)
