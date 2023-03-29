#!/bin/sh

# This what I call a real race condition :D 
kitty -e 'cmus' &
sleep 1
cmus-remote -C 'add -q ~/music/spotify' &
sleep 2
cmus-remote -C 'rand' &
