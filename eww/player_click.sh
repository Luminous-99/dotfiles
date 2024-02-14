#!/bin/bash


player_button_pause=""
player_button_play=""

case "$(playerctl --player="spotify" status)" in 
    "Playing")
    eww update player_icon="$player_button_play" 
    playerctl --player="spotify" pause
    ;;
    "Paused")
    eww update player_icon="$player_button_pause"
    playerctl --player="spotify" play
    ;;
    *)
    eww update player_icon="" 
    ;;
esac

eww update
