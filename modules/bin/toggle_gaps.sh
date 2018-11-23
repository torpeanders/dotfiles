#! /bin/env bash

# A basic script for toggling on and off i3-gaps

# simply add this line to your i3-config file to use
# bindsym $mod+g exec bash ~/bin/i3_gaps.sh

tmp_file="/tmp/.i3-gaps"

if [ ! -e "$tmp_file" ]; then
    i3-msg gaps inner all set 0
    feh --bg-fill ~/Pictures/wallpaper
    touch $tmp_file
else
    feh --bg-fill ~/Pictures/wallpaper.*
    i3-msg gaps inner all set 15
    rm $tmp_file
fi
