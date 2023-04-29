#!/bin/sh

xrandr --newmode "1920x1080_60.00"  172.80  1920 2040 2248 2576  1080 1081 1084 1118  -HSync +Vsync
xrandr --addmode DP-3 "1920x1080_60.00"

xrandr --output eDP-1 --primary --mode 1920x1080 --pos 3840x0 --rotate normal --output DP-1 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --off --output DP-3 --mode 1920x1080_60.00 --pos 0x0 --rotate normal --output HDMI-3 --off
