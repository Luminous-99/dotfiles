#!/bin/bash


send() {
    volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed "s/[^0-9]*//g")
    dunstify -i none -r 6666 -u normal "Volume" "$volume%"
}

case $1 in
    Up)
    pactl set-sink-volume @DEFAULT_SINK@ +5% 
    send

        ;;
    Down)
    pactl set-sink-volume @DEFAULT_SINK@ -5% 
    send

        ;;
    Mute)
    pactl set-sink-mute @DEFAULT_SINK@ toggle 
    send

        ;;
esac
