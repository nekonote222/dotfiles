;; python-mode をロードする
(when (autoload 'python-mode "python-mode" "Python editing mode." t)
  ;; python-mode のときのみ python-pep8 のキーバインドを有効にする
  (setq python-mode-hook
  (function (lambda ()
    (local-set-key "\C-c\ p" 'python-pep8))))
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist)))

;; flymake for python
(add-hook 'python-mode-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      ;; 昨日紹介した自作のシェルスクリプトを指定する
      ;; 各々の環境にあわせて編集してください
      (list "/usr/local/bin/pyck"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")

 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;http://d.hatena.ne.jp/sugyan/20100705/1278306885
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;; ;; カラーテーマを設定
;; (when (require 'color-theme-solarized)
;;     (color-theme-solarized-dark))


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
