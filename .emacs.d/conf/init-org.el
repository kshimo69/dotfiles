;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list "~/Dropbox/org/agenda.org"
                             "~/Dropbox/org/code-reading.org"
                             "~/Dropbox/org/mobileorg.org"
                             ))
;; (setq org-agenda-files (list org-directory))
(setq org-default-notes-file (concat org-directory "agenda.org"))
;; (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
;; (setq hl-line-face 'underline)

;; TODO keywords as workflow state
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "CALENDAR(c)"
                  "SOMEDAY(s)" "REFERENCE(r)" "PROJECT(p)"
                  "|" "DONE(d!)")
        (sequence "NEW(n)" "ASSIGNED(a!)" "|" "FIXED(f!)")
        ))
;; record DONE time
(setq org-log-done 'time)
;; repeat task
(require 'org-habit)

;; show TODOs
(setq org-agenda-custom-commands
      '(("x" "Unscheduled TODO" tags-todo "-SCHEDULED=>\"<now>\"" nil)))

;; http://d.hatena.ne.jp/rubikitch/20100819/org
(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline nil "Note")
         "** %?\nAdded: %T\n   %i\n")
        ("t" "Todo" entry
         (file+headline nil "Tasks")
         "** TODO %?\n   Added: %T\n   %a\n   %i\n")
        ;; ("b" "Bug" entry
        ;;  (file+headline nil "Inbox")
        ;;  "** TODO %?   :bug:\n   %i\n   %a\n   %t")
        ("b" "Bug" entry
         (file+headline nil "Tasks")
         "** NEW %?   :bug:\n   Added: %T\n   %a\n   %i\n")
        ;; ("i" "Idea" entry
        ;;  (file+headline nil "New Ideas")
        ;;  "** %?\n   %i\n   %a\n   %t")
        ("i" "Idea" entry
         (file+headline nil "New Ideas")
         "** SOMEDAY %?\n   Added: %T\n   %i\n")
        ("d" "Daily review" entry
         (file+headline nil "Note")
         "** Daily Review[/] :review:\n%?   DEADLINE: %t\n%[~/Dropbox/org/daily_review.txt]")
        ("w" "Weekly review" entry
         (file+headline nil "Note")
         "** Weekly Review %T[/] :review:\n%?%[~/Dropbox/org/weekly_review.txt]")
        ))

;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
;; http://d.hatena.ne.jp/tomoya/20090309/1236588957
(org-remember-insinuate)
(setq org-remember-templates
      '(("Note" ?n "** %^{Brief Description} %^g\n%?\n%i\nAdded: %T" nil "Note")
        ("Bug" ?b "** NEW %^{Brief Description} :bug%^g\n%?\n%i\n%a\nAdded: %T" nil "Tasks")
        ("Idea" ?i "** SOMEDAY %^{Brief Description} %^g\n%?\n%i\nAdded: %T" nil "New Ideas")
        ;; ("Todo" ?t "** %?\n   %i\n   %a\n   %T" nil "Tasks")
        ("Todo" ?t "** TODO %^{Brief Description} %^g\n%?\n%i\nAdded: %T" nil "Tasks")
        ("Daily review" ?d "** Daily Review[/] :review:\n%?   DEADLINE: %t\n%[~/Dropbox/org/daily_review.txt]\n" nil "Note")
        ("Weekly review" ?w "** Weekly Review %T[/] :review:\n%?%[~/Dropbox/org/weekly_review.txt]\n" nil "Note")
        ))

(defvar org-code-reading-software-name nil)
;; ~/memo/code-reading.org に記録する
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: "
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))

(defun org-code-reading-get-prefix (lang)
  (concat "[" lang "]"
          "[" (org-code-reading-read-software-name) "]"))
(defun org-remember-code-reading ()
  (interactive)
  (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (org-remember-templates
          `(("CodeReading" ?r "** %(identity prefix)%^{Brief Description}\n   %?\n   %a\n   %T"
             ,org-code-reading-file "Memo"))))
    (org-remember)))
(global-set-key (kbd "C-x m") 'org-remember-code-reading)

(defun org-next-visible-link ()
  "Move forward to the next link.
If the link is in hidden text, expose it."
  (interactive)
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-min))
    (message "Link search wrapped back to beginning of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
         (ct (org-context))
         (a (assoc :link ct))
         srch)
    (if a (goto-char (nth 2 a)))
    (while (and (setq srch (re-search-forward org-any-link-re nil t))
                (goto-char (match-beginning 0))
                (prog1 (not (eq (org-invisible-p) 'org-link))
                  (goto-char (match-end 0)))))
    (if srch
        (goto-char (match-beginning 0))
      (goto-char pos)
      (setq org-link-search-failed t)
      (error "No further link found"))))

(defun org-previous-visible-link ()
  "Move backward to the previous link.
If the link is in hidden text, expose it."
  (interactive)
  (when (and org-link-search-failed (eq this-command last-command))
    (goto-char (point-max))
    (message "Link search wrapped back to end of buffer"))
  (setq org-link-search-failed nil)
  (let* ((pos (point))
         (ct (org-context))
         (a (assoc :link ct))
         srch)
    (if a (goto-char (nth 1 a)))
    (while (and (setq srch (re-search-backward org-any-link-re nil t))
                (goto-char (match-beginning 0))
                (not (eq (org-invisible-p) 'org-link))))
    (if srch
        (goto-char (match-beginning 0))
      (goto-char pos)
      (setq org-link-search-failed t)
      (error "No further link found"))))
(define-key org-mode-map (kbd "M-n") 'org-next-visible-link)
(define-key org-mode-map (kbd "M-p") 'org-previous-visible-link)

;; mobileorg
;; http://59.106.108.77/ichiroc/20100107/1262870362
;; http://mobileorg.ncogni.to/
;; http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/
;; ;; staging for mobileorg directory
;; (setq org-mobile-directory (concat org-directory "stage/"))
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; captured file from mobileorg
(setq org-mobile-inbox-for-pull (concat org-directory "mobileorg.org"))
;; refile from mobileorg
(setq org-refile-targets '((org-agenda-files :level . 1)))
;; ;; push and pull
;; (defadvice org-mobile-pull (before org-mobile-download activate)
;;   (shell-command "~/bin/download_org.sh"))
;; (defadvice org-mobile-push (after org-mobile-upload activate)
;;   (shell-command "~/bin/upload_org.sh"))

;; ;; http://www.emacswiki.org/emacs/mobileorg
;; (defun my-org-convert-incoming-items ()
;;   (interactive)
;;   (with-current-buffer (find-file-noselect org-mobile-inbox-for-pull)
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\\* " nil t)
;;       (goto-char (match-beginning 0))
;;       (insert ?*)
;;       (forward-char 2)
;;       (insert "TODO ")
;;       (goto-char (line-beginning-position))
;;       (forward-line)
;;       (insert
;;        (format
;;         " SCHEDULED: %s
;; :PROPERTIES:
;; :ID: %s :END:
;; "
;;         (with-temp-buffer (org-insert-time-stamp (current-time)))
;;         (shell-command-to-string "uuidgen"))))
;;     (let ((tasks (buffer-string)))
;;       (erase-buffer)
;;       (save-buffer)
;;       (kill-buffer (current-buffer))
;;       (with-current-buffer (find-file-noselect "~/todo.txt")
;;         (save-excursion
;;           (goto-char (point-min))
;;           (search-forward "* CEG")
;;           (goto-char (match-beginning 0))
;;           (insert tasks))))))
;; (add-hook 'org-mobile-post-pull-hook 'my-org-convert-incoming-items)

;; ;; http://d.hatena.ne.jp/r_takaishi/20100211/1265888107
;; (defvar org-export-hatena-notation-subsection "^\\*\\* \\[.*\\]+ \\([^\t\n\r\f]*\\)$")
;; (defvar org-export-hatena-notation-quote '("[ ]*#\\+BEGIN_QUOTE" ">>" "[ ]*#\\+END_QUOTE" "<<"))
;; (defvar org-export-hatena-notation-super-pre '("[ ]*#\\+BEGIN_EXAMPLE" ">||" "[ ]*#\\+END_EXAMPLE" "||<"))
;; (defvar org-export-hatena-notation-src '("[ ]*#\\+BEGIN_SRC" ">||" "[ ]*#\\+END_SRC" "||<"))
;; (defvar org-export-hatena-notation-subsection "^\*\* \\([^\t\n\r\f]*\\)$")

;; (defun org-export-hatena-section ()
;;   (let ((section org-export-hatena-notation-section)
;;         (subsection org-export-hatena-notation-subsection)
;;         (subsubsection org-export-hatena-notation-subsubsection))
;;     (goto-char (point-min))
;;     (while (re-search-forward subsection nil t)
;;       (replace-match "*t* \\1"))))

;; (defun org-export-hatena-begin-to-end (notation)
;;   (goto-char (point-min))
;;   (while (re-search-forward (nth 0 notation) nil t)
;;     (replace-match (nth 1 notation)))
;;   (goto-char (point-min))
;;   (while (re-search-forward (nth 2 notation) nil t)
;;     (replace-match (nth 3 notation))))

;; (defun org-export-hatena (beg end)
;;   (interactive "r")

;;   (let ((diary (buffer-substring beg end))
;;         (quote org-export-hatena-notation-quote)
;;         (s-pre org-export-hatena-notation-super-pre)
;;         (src org-export-hatena-notation-src))
;;     (with-temp-buffer
;;       (pop-to-buffer (current-buffer))
;;       (insert diary)
;;       (org-export-hatena-begin-to-end quote)
;;       (org-export-hatena-begin-to-end s-pre)
;;       (org-export-hatena-begin-to-end src)
;;       (org-export-hatena-section)
;;       (setq diary (buffer-substring (point-min) (point-max))))
;;     (simple-hatena simple-hatena-default-id)
;;     (simple-hatena-mode)
;;     (goto-char (point-min))
;;     (newline)
;;     (insert diary)))

;; http://d.hatena.ne.jp/rubikitch/20101031/splitroot
;; (defun org-capture--split-root ()
;;   (interactive)
;;   (let ((display-buffer-function 'display-buffer-function--split-root)
;;         (org-capture)))


;; http://d.hatena.ne.jp/peccu/20101028/org_presentation
;; M-x auto-install-from-gist RET 509761 RET C-c C-c org-html5presentation.el RET
(require 'org-html5presentation)

(provide 'init-org)
