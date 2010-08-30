;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(setenv "PYTHONSTARTUP" "~/.pythonrc.py")

;; http://www.emacswiki.org/emacs/PythonMode
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; python
(setq py-python-command (executable-find "python"))

;; pdb
(cond
 (mac-p
  (setq pdb-path '/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/pdb.py
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
(autoload 'doctest-mode
  "doctest-mode" "Editing mode for Python Doctest examples." t)

;; Pymacs
;; http://pymacs.progiciels-bpi.ca/
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load "pymacs"
 '(add-to-list 'pymacs-load-path "~/bin"))

;; Rope
;; $ hg clone http://bitbucket.org/agr/rope/
;; $ hg clone http://bitbucket.org/agr/ropemacs/
;; $ hg clone http://bitbucket.org/agr/ropemode/
;; $ sudo easy_install rope
;; $ mv ./ropemode/ropemode ./ropemacs
;; $ sudo easy_install ropemacs
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (require 'python)
;; (define-key python-mode-map (kbd "C-c i") 'rope-code-assist)

;; Flymake

;; You must prepare epylint by hand
;; See also http://www.emacswiki.org/cgi-bin/wiki/PythonMode
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init))

;; http://d.hatena.ne.jp/se-kichi/20100324/1269384000
;; (defun flymake-pep8-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "pep8" (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.py\\'" flymake-pep8-init))

;; pycompleteとか一式
;; http://sourceforge.net/project/downloading.php?groupname=page&filename=py-mode-ext-1.0.tgz&use_mirror=jaist

;; ac-pycomplete
;; http://d.hatena.ne.jp/kitokitoki/20100828/p2
(defun ac-complete-pycomplete-pycomplete ()
  (interactive)
  (auto-complete '(ac-source-python)))
(setq ac-source-python
      '((prefix "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
        (candidates . ac-py-candidates)
        (requires . 0)))
(defun ac-py-candidates ()
  (pycomplete-pycomplete (py-symbol-near-point) (py-find-global-imports)))
;; (defun my-ac-python-mode ()
;;   (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary)))
;; (add-hook 'python-mode-hook 'my-ac-python-mode)


(add-hook 'python-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq py-indent-offset 4)
             ;; pycompleteはM-C-i(M-tab)とかで補完する
             ;; auto-completeに付けられないのかな
             (require 'pycomplete)
             ;; (add-to-list 'ac-sources 'ac-source-ropemacs)
             ;; (add-to-list 'ac-sources 'my-ac-python-mode)
             ;; (add-to-list 'ac-sources 'ac-source-python)
             ;; (add-to-list 'ac-sources 'ac-source-pycomplete)
             (flymake-mode t)
             ))

(provide 'init-python)
