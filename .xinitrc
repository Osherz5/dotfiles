(echo Welcome; while true; do date "+%a  %F %T        "; sleep 1; done) | dzen2 -p -dock -expand "left" -fg "#999999" &
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
xset r rate 300 50; exec emacs --use-exwm
