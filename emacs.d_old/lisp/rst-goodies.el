;;; rst-goodies.el

;;; Setting Sample

;; (require 'rst-goodies)

;; (add-hook 'rst-mode-hook
;;           (lambda ()
;;            (define-key rst-mode-map (kbd "C-@") 'anything-rst-toc-command)
;;            (define-key rst-mode-map (kbd "C-c C-t") 'anything-rst-toc-command)))

;; (defalias 'sc 'sphinx-compile)
;; (defalias 'so 'sphinx-browse)

;; install requires libraries:

;; `migemo'                    http://0xcc.net/migemo/
;; `anything.el'               http://www.emacswiki.org/emacs/anything.el
;; `anything-config.el'        http://www.emacswiki.org/emacs/anything-config.el
;; `anything-match-plugin.el'  http://www.emacswiki.org/emacs/anything-match-plugin.el
;; `anything-migemo.el'        http://www.emacswiki.org/emacs/anything-migemo.el

(require 'rst)
(require 'anything)
(require 'anything-migemo)

(defvar anything-rst-toc-buffer "*anything-rst-toc*")

(defvar anything-c-source-rst-toc
  `((name . ,anything-rst-toc-buffer)
    (init . (lambda ()
              (let ((data (rst-section-tree (rst-find-all-decorations))))
                (with-current-buffer (anything-candidate-buffer 'global)
                  (rst-toc-node data 0)))))
    (candidates-in-buffer)
    (action . anything-c-rst-toc-action)
    (migemo)))

(defun anything-rst-toc ()
  (interactive)
  (let* ((curbuf (current-buffer))
         (alldecos (rst-find-all-decorations))
         (sectree (rst-section-tree alldecos))
         (buf (get-buffer-create rst-toc-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (rst-toc-mode)
        (delete-region (point-min) (point-max))
        (insert (format "Table of Contents: %s\n" (or (caar sectree) "")))
        (rst-toc-node sectree 0)))
    buf))

(defun anything-c-rst-toc-action (candidate)
  (with-current-buffer (anything-rst-toc)
    (beginning-of-buffer)
    (search-forward-regexp candidate)
    (beginning-of-line)
    (rst-toc-mode-goto-section)))

(defun anything-rst-toc-display-buffer (buf)
  (delete-other-windows)
  (split-window (selected-window) nil t)
  (pop-to-buffer buf))

(defun anything-rst-toc-command ()
  (interactive)
  (let ((anything-display-function 'anything-rst-toc-display-buffer))
    (anything-other-buffer '(anything-c-source-rst-toc)
     anything-rst-toc-buffer)))

(defun sphinx-compile ()
  (interactive)
  (with-temp-directory (sphinx-top-dir "Makefile")
     (compile "make html")
     (view-buffer-other-window "*compilation*" t (lambda (dummy) (kill-buffer-and-window)))))

(defun sphinx-top-dir (name &optional dir)
  (interactive)
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (if (file-exists-p name)
        (file-name-directory (expand-file-name name))
      (unless (string= "/" (directory-file-name default-directory))
        (sphinx-top-dir name (expand-file-name ".." default-directory))))))

;; taken from http://d.hatena.ne.jp/podhmo/20101225/1293252081
(defun sphinx-browse ()
  (interactive)
  (let* ((command (concat "find " (sphinx-top-dir "Makefile") " -name 'index.html'"))
         (html (shell-command-to-string command)))
    (cond ((string= "" html) (message "%s is not found" "index.html"))
          (t (browse-url (file-truename html))))))

(defmacro* with-temp-directory (dir &body body)
  `(with-temp-buffer
     (cd ,dir)
     ,@body))

(provide 'rst-goodies)
