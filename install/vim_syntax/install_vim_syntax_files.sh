#!/bin/bash
# this should get syntax highlighting working in vim
mkdir ~/.vim > /dev/null 2> /dev/null # if this doesn't exist already
rsync -au arb_vim_files/* ~/.vim
# now write some commands to the .vimrc file so that arb filetype is detected, and macvim-ish colour scheme is used
echo "\" the following commands have been added by the install_vim_syntax_files.sh in "`pwd` >> ~/.vimrc
echo "\" start---------------" >> ~/.vimrc
echo "filetype plugin on" >> ~/.vimrc
echo "autocmd BufRead,BufNewFile *.arb set filetype=arb" >> ~/.vimrc
# uncomment this to use the macvim inspired colourscheme included with the distribution
#echo "colorscheme daltonh_macvim" >> ~/.vimrc
echo "syntax enable" >> ~/.vimrc
echo "\" stop---------------" >> ~/.vimrc
