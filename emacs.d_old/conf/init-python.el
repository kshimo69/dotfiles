;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(setenv "PYTHONSTARTUP" "~/.pythonrc.py")
(setq py-python-command (executable-find "python"))

(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
;; (autoload 'python-mode "python-mode" "Python editing mode." t)

;; for python.el (default newer emacs 22)
;; http://www.emacswiki.org/emacs/ProgrammingWithPythonDotEl
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-m") 'newline-and-indent)))

;; http://d.hatena.ne.jp/CortYuming/20101008/p1
(when (or emacs23-p emacs24-p)
  (defun python-partial-symbol ()
    "Return the partial symbol before point (for completion)."
    (let ((end (point))
          (start (save-excursion
                   (and (re-search-backward
                         (rx (or buffer-start (regexp "[^[:alnum:]._]"))
                             (group (1+ (regexp "[[:alnum:]._]"))) point)
                         nil t)
                        (match-beginning 1)))))
      (if start (buffer-substring-no-properties start end))))
  )

(defun ac-python-candidates ()
  (python-find-imports)
  (car (read-from-string
        (python-send-receive
         (format "emacs.complete(%S,%s)"
                 (python-partial-symbol)
                 python-imports)))))

(ac-define-source python
  '((candidates . ac-python-candidates)
    (prefix . (unless
                  (save-excursion
                    (re-search-backward "^import"
                                        (save-excursion
                                          (re-search-backward "^")) t))
                (let ((symbol
                       (python-partial-symbol)
                       ))
                  (if symbol
                      (save-excursion (search-backward symbol))))))
    (symbol . "py-f")))

(add-hook
 'python-mode-hook
 '(lambda ()
    (add-to-list 'ac-sources 'ac-source-python)
    (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
    ))


;; pdb
(cond
 (mac-p
  (setq pdb-path '/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/pdb.py
        gud-pdb-command-name (symbol-name pdb-path)))
 (ns-p
  (setq pdb-path '/usr/lib/python2.7/pdb.py
        gud-pdb-command-name (symbol-name pdb-path)))
 (linux-p
  (setq pdb-path '/usr/lib/python2.6/pdb.py
        gud-pdb-command-name (symbol-name pdb-path)))
 )
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; doctest
;; http://bazaar.launchpad.net/~python-mode-devs/python-mode/python-mode/files
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
;; (autoload 'doctest-mode
;;   "doctest-mode" "Editing mode for Python Doctest examples." t)
(require 'doctest-mode)

;; doc
;; https://github.com/tsgates/pylookup
;; http://taesoo.org/Opensource/Pylookup
(setq pylookup-dir "~/.emacs.d/share/pylookup")
;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; set executable file and db file
(setq pylookup-program (executable-find "pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
;; update db
;; % cd ~/.emacs.d/share/pylookup/
;; % pylookup.py -u file:///Users/kshimo69/.emacs.d/share/pylookup/python-2.7.1-docs-html
;; python document http://docs.python.org/ftp/python/doc/current/python-2.6.2-docs-html.tar.bz2
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c h") 'pylookup-lookup)))

;; Flymake
;; You must prepare epylint by hand (easy_install pylint)
;; See also http://www.emacswiki.org/cgi-bin/wiki/PythonMode
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-in-system-tempdir))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "flymake.py" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init))


(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq py-indent-offset 4)
             ;; (set (make-local-variable 'ac-sources)
             ;;      (append ac-sources
             ;;              '(ac-source-python)
             ;;              ))
             (flymake-mode t)
             ))

(provide 'init-python)
