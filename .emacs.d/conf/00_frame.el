;; 起動メッセージを表示させない
(setq inhibit-startup-message t)


;; ツールバーを非表示
(tool-bar-mode 0)


;; メニューバーを非表示
(menu-bar-mode 0)


;; スタートアップ非表示
(setq inhibbit-startup-screen t)


;; ウインドウのサイズ
(setq default-frame-alist
      (append (list '(width . 89) 
            '(height . 50))
              default-frame-alist))


;; カラム番号も表示
(column-number-mode t)


;; ;; 行番号を表示させない
;; (line-number-mode 0)


;; リージョン内の行数と文字数をモードラインに表示する（範囲指定時のみ）
(defun count-lines-and-chars ()
   (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    ;; これだとエコーエリアがチラつく
    ;;(count-lines-region (region-beginning) (region-end))
    ""))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))


;; タイトルバー
(setq frame-title-format "%f")


;; 行番号を常に表示する
(global-linum-mode t)



;; ;; リージョンの背景色を変更
;; (set-face-background 'region "darkgreen")


(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize))
;;   ;; テーマhoberに変更する
;;   (color-theme-hober))


;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))
