let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/multi-age/analysis
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +961 sprouts_import_data.qmd
badd +1016 scratch.r
badd +2 ~/.config/nvim/after/ftplugin/quarto.lua
badd +1 ~/.config/nvim/after/ftplugin/rmd.lua
badd +14 ~/.config/nvim/lua/options.lua
badd +63 ~/.config/nvim/lua/plugins/configs/telescope.lua
argglobal
%argdel
$argadd sprouts_import_data.qmd
edit sprouts_import_data.qmd
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 95 + 119) / 238)
exe 'vert 2resize ' . ((&columns * 142 + 119) / 238)
argglobal
balt scratch.r
setlocal fdm=expr
setlocal fde=nvim_treesitter#foldexpr()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
7
normal! zo
32
normal! zo
110
normal! zo
226
normal! zo
228
normal! zo
232
normal! zo
327
normal! zo
383
normal! zo
432
normal! zo
449
normal! zo
668
normal! zo
767
normal! zo
854
normal! zo
947
normal! zo
948
normal! zo
967
normal! zo
1039
normal! zo
let s:l = 961 - ((54 * winheight(0) + 32) / 64)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 961
normal! 018|
wincmd w
argglobal
if bufexists(fnamemodify("scratch.r", ":p")) | buffer scratch.r | else | edit scratch.r | endif
if &buftype ==# 'terminal'
  silent file scratch.r
endif
balt sprouts_import_data.qmd
setlocal fdm=expr
setlocal fde=nvim_treesitter#foldexpr()
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 1016 - ((4 * winheight(0) + 32) / 64)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1016
normal! 05|
wincmd w
exe 'vert 1resize ' . ((&columns * 95 + 119) / 238)
exe 'vert 2resize ' . ((&columns * 142 + 119) / 238)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
