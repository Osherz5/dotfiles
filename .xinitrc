#picom --config ~/.config/picom.conf -b
setxkbmap -layout 'us,il' -option 'grp:alt_shift_toggle'
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
#xset r rate 300 50; exec emacs --use-exwm
#xset r rate 300 50; exec awesome
export DESKTOP_SESSION=plasma
xset r rate 300 50; exec startplasma-x11
