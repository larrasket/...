#!/bin/sh

# This what I call a real race condition :D 
kitty -e 'cmus' &
sleep 3
cmus-remote -C 'add -q ~/music/spotify' &
sleep 3
cmus-remote -C 'rand' &
