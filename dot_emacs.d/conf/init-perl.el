;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; cperl-mode
(require 'cperl-mode)

;; perl-modeを使わない
(defalias 'perl-mode 'cperl-mode)

(add-hook 'cperl-mode-hook '(lambda () (setq indent-tabs-mode nil)))
(setq cperl-auto-newline nil)
(setq cperl-autoindent-on-semi t)
(setq cperl-indent-parens-as-block t)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-level 4)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-highlight-variables-indiscriminately t)
(setq auto-mode-alist (cons '("\\.pm$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pl$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cgi$" . cperl-mode) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(add-hook 'cperl-mode-hook
          (lambda ()
            (set-face-italic-p 'cperl-hash-face nil)
            ;; http://d.hatena.ne.jp/IMAKADO/20080612/1213223052
            (require 'perl-completion)
            (add-to-list 'ac-sources 'ac-source-perl-completion)
            (perl-completion-mode t)
            ))

;; flymake

;; set-perl5lib
;; http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
(require 'set-perl5lib)

;; http://unknownplace.org/memo/2007/12/21#e001
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))

(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))

(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))

(defun flymake-perl-load ()
  (interactive)
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append
                                         flymake-allowed-file-name-masks
                                         flymake-allowed-perl-file-name-masks
                                         ))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (set-perl5lib)
  (flymake-mode t))
(add-hook 'cperl-mode-hook 'flymake-perl-load)

(provide 'init-perl)
