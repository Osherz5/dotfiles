#!/bin/sh
ln -sf ~/.dotfiles/.emacs.d ~/.emacs.d

ln -sf ~/.dotfiles/.xinitrc ~/.xinitrc
ln -sf ~/.dotfiles/.Xmodmap ~/.Xmodmap

yay -S $(cat ~/.dotfiles/packages.txt)
