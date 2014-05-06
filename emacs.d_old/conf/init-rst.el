;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; Copyright (C) 2011  Kimihiko Shimomura
;; http://d.hatena.ne.jp/kitokitoki/20110403/p1
;; (auto-install-from-url "https://github.com/wakaran/rst-goodies/raw/master/rst-goodies.el")
(require 'rst)
(require 'rst-goodies)

;; http://d.hatena.ne.jp/ymotongpoo/20101106/1289007403
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(setq frame-background-mode 'dark)

(add-hook 'rst-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            ;; (define-key rst-mode-map (kbd "C-@") 'anything-rst-toc-command)
            (define-key rst-mode-map (kbd "C-c C-t") 'anything-rst-toc-command)))

(defalias 'sc 'sphinx-compile)
(defalias 'so 'sphinx-browse)

(provide 'init-rst)
;;; init-rst.el ends here
