;;; anythingの設定
;;(auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大数表示。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多い時に体感速度を速くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)
  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行する時のコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))
  (require 'anything-match-plugin nil t)
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))
  (require 'anything-show-completion nil t)
  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))
  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))
