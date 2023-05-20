#!/bin/bash

ping -q -c 1 -W 1 google.com > /dev/null
if [ $? -eq 0 ]
then
  echo "Turning off internet connection"
  sudo ip route del default
else
  echo "Turning on internet connection"
  sudo ip route add default via 192.168.1.1 dev wlp0s20f3
fi
