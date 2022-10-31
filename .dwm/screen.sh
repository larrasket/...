#!/bin/sh
xrandr
xrandr --output HDMI-1-0 --auto --mode 1920x1080  --right-of eDP-1 
feh --bg-fill ~/configs/bg.png
#xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1-0 --mode 1920x1080 --pos 1920x0 --rotate normal
