#!/bin/bash

send() {
    brightness=$(xbacklight -get | cut -d '.' -f 1)
    dunstify -i none -r 5555 -u normal "Brightness" "$brightness%"
}

case $1 in
    Up)
    xbacklight -inc 5
    send

        ;;
    Down)
    xbacklight -dec 5
    send

        ;;
esac
