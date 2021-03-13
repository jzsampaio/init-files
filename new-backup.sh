#!/bin/bash

cp ~/.emacs.d/init.el emacs.d/
rsync -r ~/.emacs.d/autoload/ emacs.d/autoload
cp ~/.zshrc zshrc
cp ~/.gitconfig gitconfig
crontab -l > user-crontab

git add .
git commit -m "Add backup created at $(TZ=GMT date +'%Y-%m-%dT%H:%M:%SZ')" && git push origin master
