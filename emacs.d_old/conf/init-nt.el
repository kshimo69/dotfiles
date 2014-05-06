;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

;; exec-path、PATH、MANPATHの追加 (下が優先)
;; http://sakito.jp/emacs/emacsshell.html
(dolist (dir (list
              "/sbin"
              "/bin"
              "/usr/sbin"
              "/usr/bin"
              "/usr/local/sbin"
              "/usr/local/bin"
              "/opt/local/sbin"
              "/opt/local/bin"
              (expand-file-name "~/local/bin")
              (expand-file-name "~/bin")
              ;;(substitute-in-file-name "/home/$USERNAME/local/bin")
              ;;(substitute-in-file-name "/home/$USERNAME/bin")
              ))
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ";" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))
(setenv "MANPATH"
        (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man"
        (getenv "MANPATH")))
(provide 'init-nt)

