;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;; 会社だったら
(when (locate-library "init-passwd")
  (require 'init-passwd))
(menu-bar-mode nil)

(if window-system
    (progn
      (menu-bar-mode t)

      (set-frame-font "Bitstream Vera Sans Mono-13")
      (set-face-attribute 'default nil
                          :family "Bitstream Vera Sans Mono"
                          :height 130)
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("MigMix 2M" . "unicode-bmp"))
      (set-fontset-font (frame-parameter nil 'font)
                        'katakana-jisx0201
                        '("MigMix 2M" . "unicode-bmp"))
      ;; (set-fontset-font
      ;;  (frame-parameter nil 'font)
      ;;  'japanese-jisx0208
      ;;  '("MigMix 2M" . "iso10646-1"))
      ;; (set-fontset-font
      ;;  (frame-parameter nil 'font)
      ;;  'japanese-jisx0212
      ;;  '("MigMix 2M" . "iso10646-1"))
      ;; (set-fontset-font
      ;;  (frame-parameter nil 'font)
      ;;  'mule-unicode-0100-24ff
      ;;  '("Bitstream Vera Sans Mono" . "iso10646-1"))
      (setq face-font-rescale-alist
            '((".*MigMix 2M.*" . 1.2)
              ("-cdac$" . 1.3)))

      ;; 最初のフレーム
      (setq initial-frame-alist
            (append (list
                     '(foreground-color . "white")
                     '(background-color . "black")
                     '(cursor-color . "orange")
                     '(mouse-color . "orange")
                     '(vertical-scroll-bars . nil)
                     '(width . 160)                  ;; フレームの幅
                     '(height . 60)                  ;; フレームの高さ
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
                     )
                    default-frame-alist)
            )
      ;; (setq-default line-spacing 1)
      ))

(provide 'init-linux)
