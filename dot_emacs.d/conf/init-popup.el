;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://d.hatena.ne.jp/khiker/20100425/popup_kill_ring
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)
(global-set-key "\M-y" 'popup-kill-ring)

;; http://www.emacswiki.org/emacs/PosTip#toc3
(require 'pos-tip)
(defun my-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) in tooltip."
  (interactive (list (function-called-at-point)))
  (if (null function)
      (pos-tip-show
       "** You didn't specify a function! **" '("red"))
    (pos-tip-show
     (with-temp-buffer
       (let ((standard-output (current-buffer))
             (help-xref-following t))
         (prin1 function)
         (princ " is ")
         (describe-function-1 function)
         (buffer-string)))
     nil nil nil 0)))
(define-key emacs-lisp-mode-map (kbd "C-c ,") 'my-describe-function)

(provide 'init-popup)
