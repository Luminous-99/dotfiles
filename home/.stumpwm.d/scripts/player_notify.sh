#!/bin/bash

set -xe

icondir="/tmp/player_notify_icons/"
artist=$(playerctl --player="$1" metadata --format "{{artist}}")
title=$(playerctl --player="$1" metadata --format "{{title}}")
album=$(playerctl --player="$1" metadata --format "{{album}}")
sartist=$(playerctl --player="$1" metadata --format "{{artist}}" | tr " " "_")
stitle=$(playerctl --player="$1" metadata --format "{{title}}" | tr " " "_")
url=$(playerctl --player="$1" metadata --format "{{mpris:artUrl}}" | tr " " "_")
icon_file=$(echo "$icondir/$sartist/$stitle")

if [ -e "$icon_file" ] || [ -z "$url" ]; then
    echo
else
    mkdir -p "$icondir/$sartist"
    curl -o "$icon_file" -L "$url"
fi

function truncate () {
    string="$1"
    count="$2"
    truncated_string="$(echo "$string" | head -c $count)"
    if [ "$truncated_string" != "$string" ]; then
        truncated_string="$truncated_string..."
    fi
    echo $truncated_string;
}

if [ -z "$2" ]; then
    if [ -z "$album" ]; then
        dunstify --icon "$icon_file" "$artist" "$title"
    else
        album=$(truncate "$album" 30)
        title=$(truncate "$title" 30)
        artist=$(truncate "$artist" 30)
        dunstify --icon "$icon_file" "$album" "$artist - $title"
    fi
else
    dunstify --icon "$icon_file" "$title" "$2"
fi
