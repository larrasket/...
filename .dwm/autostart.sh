#! /bin/bash 
picom --experimental-backends &
dunst &
nitrogen --restore &
xset r rate 200 25
feh --bg-fill s.png
setxkbmap -layout en_US,ar -option 'grp:alt_shift_toggle'
flameshot &
./home/ghd/.cargo/bin/rsblocks &
