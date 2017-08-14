#!/bin/bash
xrdb ~/.Xresources
#feh --bg-fill /home/$USER/.xmonad/bg/awan.jpg &
#feh --bg-fill /home/mel/Pictures/background/chloe.jpg &
xsetroot -cursor_name left_ptr &
urxvtd &
start-pulseaudio-x11 --log-target=syslog &
sleep 10; amixer -c 1 set Headphone 100% unmute 
wmname LG3D
