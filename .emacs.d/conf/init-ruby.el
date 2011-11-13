;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; http://blog.livedoor.jp/k1LoW/archives/65214091.html

;; http://curiosity-drives.me/programming/rails/rails_emacs_rinari_yasnippet_flymake/
;; ruby-modeはRubyの中に入ってるので、Rubyの公式サイトからダウンロードして
;; 展開、ruby/miscフォルダ以下を特定のディレクトリに配置
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;; (auto-install-from-emacswiki "ruby-block.el")
(require 'ruby-block)
(ruby-block-mode t)

;; flymake for ruby
;; Invoke ruby with '-c’ to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-in-system-tempdir))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don’t want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))))

;; rcodetools
;; $gem install rcodetools
;; $sudo ruby ~/.gem/ruby/1.9.0/gems/rcodetools-0.8.5.0/setup.rb
;; % cp ~/local/lib/rubygems/gems/rcodetools-0.8.5.0/rcodetools.el ~/.emacs.d/lisp/
;; % export PATH=$PATH:/var/lib/gems/1.8/bin

;; (auto-install-from-emacswiki "anything-rcodetools.el")
(require 'anything-rcodetools)
;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat fri -l")
;; See docs
;; (define-key anything-map (kbd "C-z") 'anything-execute-persistent-action)


;; (auto-install-from-url "http://www.cx4a.org/pub/auto-complete-ruby.el")
(add-hook 'ruby-mode-hook
          (lambda ()
            (require 'rcodetools)
            (require 'auto-complete-ruby)
            (make-local-variable 'ac-omni-completion-sources)
            (setq ac-omni-completion-sources
                  '(("\\.\\=" . (ac-source-rcodetools))))
            (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
            (add-to-list 'ac-sources 'ac-source-yasnippet)
            (add-to-list 'ac-sources 'ac-source-rcodetools)
            ))

(provide 'init-ruby)
