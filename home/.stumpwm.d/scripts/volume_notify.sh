#!/bin/bash

mode=$1
amount=$2
send() {
    volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{ print $5 }' | sed "s/[^0-9]*//g")
    muted=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{ print $2 }')

    if [ "$muted" == "yes" ];  then
        muted="";
    else 
        muted="";
    fi

    case $1 in
        Silent) ;;
        *) dunstify -i none -r 6666 -u normal -h int:value:$volume " $muted " "" ;;
    esac
    echo $volume
}

case $mode in
    Up)
	    if [ -z "$amount" ]; then
	        pactl set-sink-volume @DEFAULT_SINK@ +5% 
	    else
	        pactl set-sink-volume @DEFAULT_SINK@ +$amount%
	    fi
	    send
        ;;
    Down)
	    if [ -z "$amount" ]; then
	        pactl set-sink-volume @DEFAULT_SINK@ -5% 
	    else
	        pactl set-sinkvolume @DEFAULT_SINK@ -$amount%
	    fi
	    send
        ;;
    Mute)
	    pactl set-sink-mute @DEFAULT_SINK@ toggle 
	    send
        ;;
    Volume)
        send Silent
        ;;
esac
