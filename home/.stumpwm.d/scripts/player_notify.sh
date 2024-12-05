#!/bin/bash

set -xe

icondir="/tmp/player_notify_icons/"
artist=$(playerctl --player="$1" metadata --format "{{artist}}")
title=$(playerctl --player="$1" metadata --format "{{title}}")
album=$(playerctl --player="$1" metadata --format "{{album}}")
sartist=$(playerctl --player="$1" metadata --format "{{artist}}" | tr " " "_")
stitle=$(playerctl --player="$1" metadata --format "{{title}}" | tr " " "_")
salbum=$(playerctl --player="$1" metadata --format "{{album}}" | tr " " "_")
url=$(playerctl --player="$1" metadata --format "{{mpris:artUrl}}" | tr " " "_")
icon_file=$(echo "$icondir/$sartist/$stitle")

if [ -e "$icon_file" ]; then
    echo
else
    mkdir -p "$icondir/$sartist"
    curl -o "$icon_file" -L "$url"
fi

if [ -z "$2" ]; then
    if [ -z "$album" ]; then
	dunstify --icon "$icon_file" "$title" "$artist"
    else
	dunstify --icon "$icon_file" "$title" "$artist - $album"
    fi
else
    dunstify --icon "$icon_file" "$title" "$2"
fi
