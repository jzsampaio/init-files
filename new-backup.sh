#!/bin/bash

cp ~/.emacs.d/init.el emacs.d/
rsync -r ~/.emacs.d/autoload/ emacs.d/autoload
cp ~/.zshrc zshrc
cp ~/.gitconfig gitconfig
