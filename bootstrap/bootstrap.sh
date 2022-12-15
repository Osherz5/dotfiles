#!/bin/sh
ln -sf ~/.dotfiles/.emacs.d ~/.emacs.d
ln -sf ~/.dotfiles/awesome ~/.config/awesome

ln -sf ~/.dotfiles/.xinitrc ~/.xinitrc
ln -sf ~/.dotfiles/.Xmodmap ~/.Xmodmap
ln -sf ~/.dotfiles/picom.conf ~/.config/picom.conf 


yay -S $(cat ~/.dotfiles/packages.txt)
