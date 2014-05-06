;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://www.nongnu.org/baol-hth/
(require 'html-helper-mode)
;; http://coderepos.org/share/wiki/SimpleHatenaMode
(require 'simple-hatena-mode)
;; http://d.hatena.ne.jp/amt/20060115/HatenaHelperMode
(require 'hatenahelper-mode)
(setq simple-hatena-default-id "kshimo69")
(setq simple-hatena-bin "~/bin/hw.pl")
(setq simple-hatena-time-offset 6)
(add-hook 'simple-hatena-mode-hook
          '(lambda ()
             (hatenahelper-mode 1)))

(provide 'init-hatena)
