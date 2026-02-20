#!/bin/bash

clicked_button=$1

eww update "wbutton$clicked_button"="active-workspace-button"

for i in {1..10};
do 
    if [ "$i" != "$clicked_button" ]; then
        eww update "wbutton$i"="workspace-button"
    fi
done
