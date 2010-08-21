;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; http://www.emacswiki.org/emacs/emacs-w3m
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key (kbd "C-x m") 'browse-url-at-point)
(setq w3m-use-cookies t)
;; (setq w3m-cookie-accept-bad-cookies 'ask)
(setq w3m-cookie-accept-bad-cookies t)
;; (setq w3m-display-inline-image t)
(setq w3m-default-display-inline-images t)

(autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
;; w3m-search-engine-alistの内容を確認してください
(setq w3m-search-default-engine "google")
(global-set-key (kbd "C-c s") 'w3m-search)

(autoload 'w3m-weather "w3m-weather" "Display weather report." t)
(setq w3m-weather-default-area "神奈川県・東部")
(autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)
(setq w3m-use-form t)

(provide 'init-w3m)
