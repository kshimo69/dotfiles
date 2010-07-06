;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'taskpaper)

;; taskpaperファイルを置くディレクトリ
(setq taskpaper-dir (expand-file-name "~/.taskpaper/"))

;; `M-x taskpaper'で、今日の日付けのファイルを新しいフレームで開く
(defun taskpaper ()
  (interactive)
  (let ((filename (concat taskpaper-dir
                          (format-time-string "%Y-%m-%d.taskpaper"))))
    (find-file-other-frame filename)))

(provide 'init-taskpaper)
