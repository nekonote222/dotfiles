;; キーバインド
(global-set-key (kbd "C-x C-r") 'recentf-open-files)


;; 好きなコマンドを割り振ろう
;;(global-set-key (kbd "C-x C-c") 'helm-M-x)

;; C-x C-z(suspend)も変更するのもありでしょう.
;(global-set-key (kbd "C-x C-z") 'your-favorite-command)

;; I never use C-x C-c
(defalias 'exit 'save-buffers-kill-emacs)

;; C-mにnewline-and-indentを割りあてる。初期値はnewline
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-j") 'newline-and-indent)


;; C-mにnewline-and-indentを割りあてる
;;(global-set-key (kbd "C-m") 'newline-and-indent)

;; C-Ret で矩形選択
;;詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; C-h にバックスペースを割りあてる
;; C-x ? に元のキーバインドを割りあてる
(keyboard-translate ?\C-h ?\C-?)
(define-key global-map (kbd "C-x ?") 'help-command)


;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; C-t でウィンドウを切り替える。初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)
