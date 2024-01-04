#! /bin/bash
for f in /etc/X11/xinit/xinitrc.d/*.sh; do
    source "$f"
done
~/.dwm/screen.sh
~/.dwm/keyboard.sh
dunst &
feh --bg-fill ~/configs/~s/bg.png &
flameshot &
picom --xrender-sync-fence --vsync &
rygel -g 5 &
