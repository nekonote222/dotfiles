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

;; package-installのレポジトリを追加
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; 初期化
;;(package-initialize)


;; キーバインド
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; 文字を見やすくする
(when (eq window-system 'ns)
  (let ((my-font-height 120)
        (my-font (cond
                  (t   "Monaco")  ;; XCode 3.1 で使っているフォント
                  (nil "Menlo")   ;; XCode 3.2 で使ってるフォント
                  ))
        (my-font-ja "Hiragino Maru Gothic Pro"))
    (setq mac-allow-anti-aliasing t)

    ;; ;; フォントサイズの微調節 (12ptで合うように)
    ;; (setq face-font-rescale-alist
    ;;       '(("^-apple-hiragino.*" . 1.2)
    ;;         (".*osaka-bold.*" . 1.2)
    ;;         (".*osaka-medium.*" . 1.2)
    ;;         (".*courier-bold-.*-mac-roman" . 1.0)
    ;;         (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
    ;;         (".*monaco-bold-.*-mac-roman" . 0.9)
    ;;         ("-cdac$" . 1.3)))
    
    ;; ;; デフォルトフォント設定
    ;; (when my-font
    ;;   (set-face-attribute 'default nil :family my-font :height my-font-height)
    ;;   ;;(set-frame-font (format "%s-%d" my-font (/ my-font-height 10)))
    ;;   )
    
    ;; 日本語文字に別のフォントを指定
    (when my-font-ja
      (let ((fn (frame-parameter nil 'font))
            (rg "iso10646-1"))
        (set-fontset-font fn 'katakana-jisx0201 `(,my-font-ja . ,rg))
        (set-fontset-font fn 'japanese-jisx0208 `(,my-font-ja . ,rg))
        (set-fontset-font fn 'japanese-jisx0212 `(,my-font-ja . ,rg))))))



 
;; 好きなコマンドを割り振ろう
(global-set-key (kbd "C-x C-c") 'helm-M-x) ;; 私は helm-M-xにしています

;; C-x C-z(suspend)も変更するのもありでしょう.
(global-set-key (kbd "C-x C-z") 'your-favorite-command)

;; I never use C-x C-c
(defalias 'exit 'save-buffers-kill-emacs)

(require 'server)

(unless (server-running-p) ;; 複数サーバ起動を防ぐ
  (server-start))

;;; 外観に関する設定----------


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




;; paren-mode : 対応する括弧を強調して表示する
(setq show-paren-delay 0) ; 表示までの秒数。初期値は0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)
;; フェイスを変更する
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "blue")




;; http://coderepos.org/share/browser/lang/elisp/init-loader/init-loader.el
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ; 設定ファイルがあるディレクトリを指定


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


;; TABの表示幅。初期値は8
(setq-default tab-width 4)


;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)


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


;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))


;; ;; asciiフォントをMenloにする
;; (set-face-attribute 'default nil
;;                     :family "Menlo"
;;                     :height 120)


;; ;; 日本語フォントをヒラギノ明朝 Proに
;; (set-fontset-font
;;  nil 'japanese-jisx0208
;;  ;; 英語名の場合
;;  ;; (font-spec :family "Hiragino Mincho Pro"))
;;  (font-spec:family "ヒラギノ明朝 Pro"))


;; ;; ひらがなとカタカナをモトヤシーダにする
;; ;; U+3000-303F CJKの記号および句読点
;; ;; U+3040-309F ひらがな
;; ;; U+30A0-30FF カタカナ
;; (set-fontset-font
;;  nil '(#x3040 . #x30ff)
;;  (font-spec :family "NfMotoyaCedar"))


;; (setq face-font-rescale-alist
;;       '((".*Menlo.*" . 1.0)
;;         (".*Hiragino_Mincho_Pro.*" . 1.2)
;;         (".*nfmotoyacedar-bold.*" . 1.2)
;;         (".*nfmotoyacedar-medium.*" . 1.2)
;;         ("-cdac$" . 1.3)))



;; ;; バックアップファイルを作成しない
;; (setq make-backup-files nil) ; 初期値はt
;; ;; オートセーブファイルを作らない
;; (setq auto-save-default nil) ; 初期値はt


;; バックアップとオートセーブファイルを~/.emacs.d/backup/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backup/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backup/") t)))
;; ロックファイルを作らない
(setq create-lockfiles nil)

;; オートセーブファイル作成までの秒間隔
(setq auto-save-timeout 15)


;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)


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


;; redo+.elのインストールをした
;;(install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")
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




;;トラックパッド用のスクロール設定
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 2))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 2))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)


;; ;; auto-comleteの設定
;; (when (require 'auto-completeconfig nil t)
;;   (add-to-list 'ac-dictionary-directories
;;                "~/.emacs.d/elisp/ac-dict")
;;   (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;;   (ac-config-default))


;; ;; (require 'auto-complete)
;; ;; (require 'auto-complete-config)
;; ;; (global-auto-complete-mode t)


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

;; ;; python-mode をロードする
;; (when (autoload 'python-mode "python-mode" "Python editing mode." t)
;;   ;; python-mode のときのみ python-pep8 のキーバインドを有効にする
;;   (setq python-mode-hook
;;   (function (lambda ()
;;     (local-set-key "\C-c\ p" 'python-pep8))))
;;   (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;;   (setq interpreter-mode-alist (cons '("python" . python-mode)
;;                                      interpreter-mode-alist)))

;; ;; flymake for python
;; (add-hook 'python-mode-hook 'flymake-find-file-hook)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       ;; 昨日紹介した自作のシェルスクリプトを指定する
;;       ;; 各々の環境にあわせて編集してください
;;       (list "/usr/local/bin/pyck"  (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (load-library "flymake-cursor")

;;  (autoload 'python-mode "python-mode" "Python Mode." t)
;;  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;  (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ;; カラーテーマを設定
;; (when (require 'color-theme-solarized)
;;     (color-theme-solarized-dark))


;; (defface my-hl-line-face
;;   ;; 背景がdarkならば背景色を紺に
;;   '((((class color) (background dark))
;;      (:background "NavyBlue" t))
;;     ;; 背景がlightならば背景色を緑に
;;     (((class color) (background light))
;;      (:background "LightGoldenrodYellow" t))
;;     (t (:bold t)))
;;   "hl-line's my face")
;; (setq hl-line-face 'my-hl-line-face)
;; (global-hl-line-mode t)
