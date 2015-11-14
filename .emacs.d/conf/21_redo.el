;;忘備録(install-elisp "http://www.emacswiki.org/emacs/download/redo+.el)"
;; redo+の設定
(when (require 'redo+ nil t)
  ;; C-' にリデゥを割りあてる
  ;;(global-set-key (kbd "C-'") 'redo)
  ;; 日本語キーボードの場合 C-. などが良いかも
  (global-set-key (kbd "C-.") 'redo)
  )
