;; バックアップとオートセーブファイルを~/.emacs.d/backup/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backup/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))

;; オートセーブファイル作成までの秒間隔
(setq auto-save-timeout 15)


;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)
