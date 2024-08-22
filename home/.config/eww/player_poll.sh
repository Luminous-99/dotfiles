#!/bin/bash


player_icon_pause=""
player_icon_play=""

if   [ "$(playerctl --player="spotify" status)" = "Playing" ]; then 
    echo $player_icon_pause
elif [ "$(playerctl --player="spotify" status)" = "Paused" ]; then
    echo $player_icon_play
fi
