;; moccur-editの設定
(require 'omccur-edit nil t)

;; moccur-edit-finish-editと同時にファイルを保存する
(defadvice moccur-edit-chage-file
    (after save-after-moccur-edit-buffer activate)
  (save-buffer))
