picom --config ~/.config/picom.conf -b
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
#xset r rate 300 50; exec emacs --use-exwm
xset r rate 300 50; exec awesome
