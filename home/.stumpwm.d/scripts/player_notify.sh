#!/bin/bash

set -xe

icondir="/tmp/player_notify_icons/"
artist=$(playerctl --player="$1" metadata --format "{{artist}}")
title=$(playerctl --player="$1" metadata --format "{{title}}")
album=$(playerctl --player="$1" metadata --format "{{album}}")
url=$(playerctl --player="$1" metadata --format "{{mpris:artUrl}}")
icon_file=$(echo "$icondir/$artist/$title" | tr " " "_")

if [ -e "$icon_file" ]; then
    echo
else
    mkdir -p "$icondir/$artist"
    curl -L -o "$icon_file" "$url"
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
