let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/nixconfig
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +1 home.nix
badd +0 nvim.nix
argglobal
%argdel
$argadd .
edit home.nix
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
wincmd _ | wincmd |
split
wincmd _ | wincmd |
split
2wincmd k
wincmd w
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
exe 'vert 1resize ' . ((&columns * 30 + 127) / 255)
exe 'vert 2resize ' . ((&columns * 111 + 127) / 255)
exe '3resize ' . ((&lines * 23 + 37) / 74)
exe 'vert 3resize ' . ((&columns * 112 + 127) / 255)
exe '4resize ' . ((&lines * 23 + 37) / 74)
exe 'vert 4resize ' . ((&columns * 112 + 127) / 255)
exe '5resize ' . ((&lines * 23 + 37) / 74)
exe 'vert 5resize ' . ((&columns * 112 + 127) / 255)
argglobal
enew
file NvimTree_1
balt home.nix
setlocal fdm=manual
setlocal fde=
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
wincmd w
argglobal
balt nvim.nix
setlocal fdm=manual
setlocal fde=
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 19 - ((18 * winheight(0) + 35) / 70)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 19
normal! 03|
lcd ~/nixconfig
wincmd w
argglobal
if bufexists(fnamemodify("~/nixconfig/nvim.nix", ":p")) | buffer ~/nixconfig/nvim.nix | else | edit ~/nixconfig/nvim.nix | endif
if &buftype ==# 'terminal'
  silent file ~/nixconfig/nvim.nix
endif
balt ~/nixconfig/home.nix
setlocal fdm=manual
setlocal fde=
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 20 - ((6 * winheight(0) + 11) / 22)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 20
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("~/nixconfig/nvim.nix", ":p")) | buffer ~/nixconfig/nvim.nix | else | edit ~/nixconfig/nvim.nix | endif
if &buftype ==# 'terminal'
  silent file ~/nixconfig/nvim.nix
endif
balt ~/nixconfig/home.nix
setlocal fdm=manual
setlocal fde=
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let &fdl = &fdl
let s:l = 19 - ((8 * winheight(0) + 11) / 22)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 19
normal! 0
wincmd w
argglobal
enew | setl bt=help
help views-sessions@en
balt ~/nixconfig/nvim.nix
setlocal fdm=manual
setlocal fde=
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal nofen
silent! normal! zE
let &fdl = &fdl
let s:l = 787 - ((14 * winheight(0) + 11) / 23)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 787
normal! 026|
lcd /nix/store/rf2w155phn9alfh610ldhbyrsnfcczrg-neovim-unwrapped-0.9.4/share/nvim/runtime/doc
wincmd w
2wincmd w
exe 'vert 1resize ' . ((&columns * 30 + 127) / 255)
exe 'vert 2resize ' . ((&columns * 111 + 127) / 255)
exe '3resize ' . ((&lines * 23 + 37) / 74)
exe 'vert 3resize ' . ((&columns * 112 + 127) / 255)
exe '4resize ' . ((&lines * 23 + 37) / 74)
exe 'vert 4resize ' . ((&columns * 112 + 127) / 255)
exe '5resize ' . ((&lines * 23 + 37) / 74)
exe 'vert 5resize ' . ((&columns * 112 + 127) / 255)
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
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
