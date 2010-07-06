;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 会社だったら
(setq url-proxy-services '(("http" . "10.209.149.1:8080")))
(when (locate-library "init-passwd")
  (require 'init-passwd))
(menu-bar-mode nil)

(if window-system
    (progn
      (menu-bar-mode t)
      ;; ;; GUIの場合はM+2VM+IPAG cicleを指定
      ;; (set-default-font "M+2VM+IPAG circle-16")
      ;; (set-frame-font "M+2VM+IPAG circle-16")
      ;; ;; (set-default-font "VL ゴシック-14")
      ;; ;; (set-frame-font "VL ゴシック-14")
      ;; ;; (set-frame-font "Bitstream Vera Sans Mono-14")
      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'japanese-jisx0208
      ;;                   '("M+2VM+IPAG circle-16" . "unicode-bmp"))
      ;;                   ;; '("VL ゴシック-14" . "unicode-bmp"))
      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'katakana-jisx0201
      ;;                   '("M+2VM+IPAG circle-16" . "unicode-bmp"))
      ;;                   ;; '("VL ゴシック-14" . "unicode-bmp"))
      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'ascii
      ;;                   '("M+2VM+IPAG circle-16" . "unicode-bmp"))
      ;;                   ;; '("VL ゴシック-14" . "unicode-bmp"))
      ;; (set-fontset-font (frame-parameter nil 'font)
      ;;                   'unicode
      ;;                   '("M+2VM+IPAG circle-16" . "unicode-bmp"))
      ;;                   ;; '("VL ゴシック-14" . "unicode-bmp"))

      ;; http://emacs.g.hatena.ne.jp/sakito/20100127
      (create-fontset-from-ascii-font "VL ゴシック-14:weight=normal:slant=normal:spacing=m" nil "vlipagothic")
      (set-fontset-font "fontset-vlipagothic"
                        'unicode
                        (font-spec :family "M+2VM+IPAG circle" :spacing 'm)
                        nil
                        'append)
      ;; ひらがな、カタカナ は M+2VM+IPAG circle
      (set-fontset-font "fontset-vlipagothic"
                        '( #x3000 .  #x30ff)
                        (font-spec :family "M+2VM+IPAG circle" :spacing 'm)
                        nil
                        'prepend)
      ;; 半角カタカナ、全角アルファベット は M+2VM+IPAG circle
      (set-fontset-font "fontset-vlipagothic"
                        '( #xff00 .  #xffef)
                        (font-spec :family "M+2VM+IPAG circle" :spacing 'm)
                        nil
                        'prepend)
      (add-to-list 'default-frame-alist '(font . "fontset-vlipagothic"))

      ;; 最初のフレーム
      (setq initial-frame-alist
            (append (list
                     '(foreground-color . "white")
                     '(background-color . "black")
                     '(cursor-color . "orange")
                     '(mouse-color . "orange")
                     '(vertical-scroll-bars . nil)
                     '(width . 200)                   ;; フレームの幅
                     '(height . 50)                  ;; フレームの高さ
                     '(top . 50)                     ;; Y 表示位置
                     '(left . 200)                   ;; X 表示位置
                     '(alpha . (85 20))
                     ;; '(alpha . (50 50 50 50))        ;; 透明にする
                                        ;(通常 ノンアクティブ 移動中 サイズ変更中)
                     )
                    initial-frame-alist)
            )
      ;; 他のフレーム
      (setq default-frame-alist
            (append (list
                     '(foreground-color . "white")
                     '(background-color . "black")
                     '(cursor-color . "orange")
                     '(mouse-color . "orange")
                     '(vertical-scroll-bars . nil)
                     '(width . 80)                   ;; フレームの幅
                     '(height . 35)                  ;; フレームの高さ
                     ;; '(font . "M+2VM+IPAG circle-16")
                     ;; '(font . "VL ゴシック-14")
                     )
                    default-frame-alist)
            )
      (setq-default line-spacing 1)
      ))

(provide 'init-linux)
