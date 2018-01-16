#!/bin/bash

# this script just installs the macports packages that are required for the arb wiki
# this file is specific to osx

# eventually this may be integrated with the main install files

port install pandoc haskell-platform

# install vimhl
# https://github.com/lyokha/vim-publish-helper#customizing-vim-settings
# cabal update
# cabal install cabal-install
# cabal install pandoc-vimhl
