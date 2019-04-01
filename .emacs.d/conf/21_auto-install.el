;; auto-installの設定
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  ;; www.emacswiki.org:443のエラーが出た時は、コメントアウトすると治る。
  ;; そうでないと、起動にとても長い時間がかかる。
  ;; コメントアウトせずにエラーを解消する方法は模索中
  ;;(auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  (setq url-proxy-service '(("http" . "localhost:8339")))
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))
