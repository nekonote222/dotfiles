;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos" "backup" "elpa")


;;; 初期設定ファイルを読み込む
;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
;; 最近使ったファイルを表示する
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定


;; paren-mode : 対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "blue")


;;半角、全角スペースの可視化
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")


;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  (setq url-proxy-service '(("http" . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))



;; redo+の設定
(when (require 'redo+ nil t)
  ;; C-' にリデゥを割りあてる
  ;;(global-set-key (kbd "C-'") 'redo)
  ;; 日本語キーボードの場合 C-. などが良いかも
  (global-set-key (kbd "C-.") 'redo)
  )


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






;; ;; auto-comleteの設定
;; (when (require 'auto-completeconfig nil t)
;;   (add-to-list 'ac-dictionary-directories
;;                "~/.emacs.d/elisp/ac-dict")
;;   (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;;   (ac-config-default))


;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (global-auto-complete-mode t)


;; ;; color-moccurの設定
;; (when (require 'color-moccur nil t)
;;   ;; M-oにoccur-by-moccurを割り当て
;;   (define-key global-map (kbd "M-o") 'occur-by-moccur)
;;   ;; スペース区切りでAND検索
;;   (setq moccur-split-word t)
;;   ;; ディレクトリ検索のとき除外するファイル
;;   (add-to-list 'dmoccur-exclusion-mask "//.DS_Store")
;;   (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
;;   ;; Migemoを利用できる環境であればMigemoを使う
;;   (when (and (executable-find "cmigemo")
;;              (require 'migemo nil t))
;;     (setq moccur-use-migemo t)))


;; ;; moccur-editの設定
;; (require 'omccur-edit nil t)


;; ;; moccur-edit-finish-editと同時にファイルを保存する
;; (defadvice moccur-edit-change-file
;;     (after save-after-moccur-edit-buffer activate)
;;   (save-buffer))


;; ;; undohistの設定
;; (when (require 'undohist nil t)
;;   (undohist-initialize))


;; ;; point-undoの設定
;; (when (require 'point-undo nil t)
;;   (define-key global-map [f5] 'point-undo)
;;   (define-key global-map [f6] 'point-redo)  
;;   ;; 筆者のオススメキーバインド
;;   ;; (define-key global-map (kbd "M-[") 'point-undo)
;;   ;; (define-key global-map (kbd "M-]") 'point-redo)
;;   )


;; ;; cua-modeの設定
;; (cua-mode t) ; cua-modeをオン
;; (setq cua-enable-cua-keys)

;; ;; ;; python-mode をロードする
;; ;; (when (autoload 'python-mode "python-mode" "Python editing mode." t)
;; ;;   ;; python-mode のときのみ python-pep8 のキーバインドを有効にする
;; ;;   (setq python-mode-hook
;; ;;   (function (lambda ()
;; ;;     (local-set-key "\C-c\ p" 'python-pep8))))
;; ;;   (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;; ;;   (setq interpreter-mode-alist (cons '("python" . python-mode)
;; ;;                                      interpreter-mode-alist)))

;; ;; ;; flymake for python
;; ;; (add-hook 'python-mode-hook 'flymake-find-file-hook)
;; ;; (when (load "flymake" t)
;; ;;   (defun flymake-pyflakes-init ()
;; ;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; ;;                        'flymake-create-temp-inplace))
;; ;;            (local-file (file-relative-name
;; ;;                         temp-file
;; ;;                         (file-name-directory buffer-file-name))))
;; ;;       ;; 昨日紹介した自作のシェルスクリプトを指定する
;; ;;       ;; 各々の環境にあわせて編集してください
;; ;;       (list "/usr/local/bin/pyck"  (list local-file))))
;; ;;   (add-to-list 'flymake-allowed-file-name-masks
;; ;;                '("\\.py\\'" flymake-pyflakes-init)))
;; ;; (load-library "flymake-cursor")

;; ;;  (autoload 'python-mode "python-mode" "Python Mode." t)
;; ;;  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; ;;  (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ;; ;; カラーテーマを設定
;; ;; (when (require 'color-theme-solarized)
;; ;;     (color-theme-solarized-dark))


;; ;; (defface my-hl-line-face
;; ;;   ;; 背景がdarkならば背景色を紺に
;; ;;   '((((class color) (background dark))
;; ;;      (:background "NavyBlue" t))
;; ;;     ;; 背景がlightならば背景色を緑に
;; ;;     (((class color) (background light))
;; ;;      (:background "LightGoldenrodYellow" t))
;; ;;     (t (:bold t)))
;; ;;   "hl-line's my face")
;; ;; (setq hl-line-face 'my-hl-line-face)
;; ;; (global-hl-line-mode t)
