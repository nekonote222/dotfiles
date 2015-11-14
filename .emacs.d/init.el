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


;; #!から始まるファイルの保存時に実行権限を与える
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;; カーソル位置にあるElisp関数や変数の情報をエコーエリアへ表示させる
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)


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
