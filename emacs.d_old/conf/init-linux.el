;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(menu-bar-mode nil)

(if window-system
    (progn
      (menu-bar-mode nil)

      ;; (set-frame-font "ricty-12")
      ;; ;; (add-to-list 'default-frame-alist '(font . "ricty-16"))
      ;; (set-face-attribute 'default nil
      ;;                     ;; :family "Bitstream Vera Sans Mono"
      ;;                     :family "ricty"
      ;;                     :height 130)
      (set-frame-font "DejaVu LGC Sans Mono-12")
      (set-face-attribute 'default nil
                          ;; :family "Bitstream Vera Sans Mono"
                          :family "DejaVu LGC Sans Mono"
                          :height 120)
      (set-fontset-font (frame-parameter nil 'font)
                        'japanese-jisx0208
                        '("Migu 1M" . "unicode-bmp"))
                        ;; '("MigMix 2M" . "unicode-bmp"))
                        ;; '("IPA Gothic" . "unicode-bmp"))
                        ;; '("VL Gothic" . "unicode-bmp"))
      (set-fontset-font (frame-parameter nil 'font)
                        'katakana-jisx0201
                        '("Migu 1M" . "unicode-bmp"))
                        ;; '("MigMix 2M" . "unicode-bmp"))
                        ;; '("IPA Gothic" . "unicode-bmp"))
                        ;; '("VL Gothic" . "unicode-bmp"))
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
              (".*MigMix 1M.*" . 1.2)
              (".*Migu 1M.*" . 1.2)
              (".*IPA Gothic.*" . 1.2)
              (".*VL Gothic.*" . 1.2)
              ("-cdac$" . 1.3)))

      ;; 最初のフレーム
      (setq initial-frame-alist
            (append (list
                     '(foreground-color . "white")
                     '(background-color . "black")
                     '(cursor-color . "orange")
                     '(mouse-color . "orange")
                     '(vertical-scroll-bars . nil)
                     '(width . 164)                  ;; フレームの幅
                     '(height . 72)                  ;; フレームの高さ
                     '(top . 50)                     ;; Y 表示位置
                     '(left . 650)                   ;; X 表示位置
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
                     '(width . 82)                   ;; フレームの幅
                     '(height . 38)                  ;; フレームの高さ
                     )
                    default-frame-alist)
            )
      ;; (setq-default line-spacing 1)
      ))

(provide 'init-linux)