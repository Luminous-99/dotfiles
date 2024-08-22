#!/bin/bash

if [ "$(playerctl status)" = "Playing" ]; then 
    playerctl --follow metadata --format '{{ artist }} - {{ title }}'
else 
    echo "1"
fi
