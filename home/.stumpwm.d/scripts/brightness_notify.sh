#!/bin/bash

mode=$1
amount=$2
send() {
    max=$(brightnessctl max)
    current=$(brightnessctl get)
    brightness=$(echo "scale=2; ($current/$max)*100" | bc)
    dunstify -i none -r 5555 -u normal -h int:value:$brightness "  " ""
}

case $mode in
    Up)
	if [ -z "$amount" ]; then
	    brightnessctl set +5%
	else		
	    brightnessctl set +$amount%
	fi
	send
        ;;
    Down)
	if [ -z "$amount" ]; then
	    brightnessctl set 5%-
	else
	    brightnessctl set $amount%-
	fi
	send
        ;;
esac
