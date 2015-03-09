#!/bin/bash
# this should get syntax highlighting working in vim
mkdir ~/.vim > /dev/null 2> /dev/null # if this doesn't exist already
rsync -au arb_vim_files/* ~/.vim
echo "filetype plugin on" >> ~/.vimrc
echo "autocmd BufRead,BufNewFile *.arb set filetype=arb" >> ~/.vimrc
echo "syntax enable" >> ~/.vimrc
