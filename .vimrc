" insertモードから抜ける
inoremap <silent> jj <ESC>
inoremap <silent> <C-j> j
inoremap <silent> kk <ESC>
inoremap <silent> <C-k> k

" 行頭・行末移動方向をキーの相対位置にあわせる
nnoremap 0 $ 
nnoremap 1 0 

" 挿入モードでのカーソル移動
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-h> <Left>
inoremap <C-l> <Right>

" カーソル前の文字削除
""inoremap <silent> <C-h> <C-g>u<C-h>
" カーソル後の文字削除
""inoremap <silent> <C-d> <Del>
" カーソルから行頭まで削除
""inoremap <silent> <C-d>e <Esc>lc^
" カーソルから行末まで削除
""inoremap <silent> <C-d>0 <Esc>lc$
" カーソルから行頭までヤンク
""inoremap <silent> <C-y>e <Esc>ly0<Insert>
" カーソルから行末までヤンク
""inoremap <silent> <C-y>0 <Esc>ly$<Insert>

" 引用符, 括弧の設定
inoremap { {}<Left>
inoremap [ []<Left>
inoremap ( ()<Left>
inoremap " ""<Left>
inoremap ' ''<Left>
inoremap <> <><Left>

