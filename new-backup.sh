#!/bin/bash

cp ~/.emacs.d/init.el emacs.d/
cp ~/.emacs.d/settings.org  emacs.d/
rsync -r ~/.emacs.d/autoload/ emacs.d/autoload
rsync -r ~/.emacs.d/lisp/ emacs.d/lisp
cp ~/.zshrc zshrc
cp ~/.gitconfig gitconfig
crontab -l > user-crontab

git add .
git commit -m "Add backup created at $(TZ=GMT date +'%Y-%m-%dT%H:%M:%SZ')" && git push origin master
