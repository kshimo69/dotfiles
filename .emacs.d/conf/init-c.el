;;; init-c.el --- 

;; Copyright (C) 2011  Kimihiko Shimomura

;; Author: Kimihiko Shimomura <kshimo69@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun flymake-c-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-in-system-tempdir))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "gcc" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.c$" flymake-c-init) flymake-allowed-file-name-masks)

(add-hook 'c-mode-hook
          '(lambda ()
             (flymake-mode t)))

(defun flymake-cc-init ()
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-in-system-tempdir))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c++-mode-hook
          '(lambda ()
             (flymake-mode t)))

(add-hook 'c-mode-common-hook '(lambda ()
          ;; ac-omni-completion-sources is made buffer local so
          ;; you need to add it to a mode hook to activate on 
          ;; whatever buffer you want to use it with.  This
          ;; example uses C mode (as you probably surmised).
          ;; auto-complete.el expects ac-omni-completion-sources to be
          ;; a list of cons cells where each cell's car is a regex
          ;; that describes the syntactical bits you want AutoComplete
          ;; to be aware of. The cdr of each cell is the source that will
          ;; supply the completion data.  The following tells autocomplete
          ;; to begin completion when you type in a . or a ->
          (add-to-list 'ac-omni-completion-sources
                       (cons "\\." '(ac-source-semantic)))
          (add-to-list 'ac-omni-completion-sources
                       (cons "->" '(ac-source-semantic)))
          ;; ac-sources was also made buffer local in new versions of
          ;; autocomplete.  In my case, I want AutoComplete to use 
          ;; semantic and yasnippet (order matters, if reversed snippets
          ;; will appear before semantic tag completions).
          (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
          ))

(add-hook 'c++-mode
          (lambda () (add-to-list
                      'ac-sources
                      'ac-source-semantic
                      )))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-gtags
                             ) ac-sources)))

(add-hook 'c-mode-hook
          '(lambda ()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             ))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode t)
            ))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  ;; grr - still does not seem to work properly
  (make-variable-buffer-local 'tab-width)
  (make-variable-buffer-local 'indent-tabs-mode)
  (make-variable-buffer-local 'c-basic-offset)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))
(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
                       auto-mode-alist))
(push '(".*/linux.*/.*\\.[ch]\\'" . linux-c-mode) auto-mode-alist)
(push '(".*/linux.*/.*\\.[ch]\\(-[a-z]+\\)?\\'" . linux-c-mode) auto-mode-alist)

(provide 'init-c)
;;; init-c.el ends here
