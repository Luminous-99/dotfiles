#!/usr/bin/env bash

mode="$1"
amount="$2"
send() {
    volume=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{ print $5 }' | sed "s/[^0-9]*//g")
    muted=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{ print $2 }')

    mute_symbol=""
    if [ "$muted" == "yes" ];  then
        mute_symbol="";
    else 
        mute_symbol="";
    fi

    case $1 in
        Silent) ;;
        *) dunstify -i none -r 6666 -u normal -h int:value:$volume " $mute_symbol " "" ;;
    esac
    if [ "$muted" = "yes" ]; then
        echo $mute_symbol
    else
        echo $volume
    fi
}

case "$mode" in
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
    IsMuted?)
	pactl get-sink-mute @DEFAULT_SINK@ 
    send Silent
    ;;
    Volume)
        send Silent
        ;;
esac
