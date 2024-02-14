#!/bin/bash

menu_icon_down="󰍝"
menu_icon_up="󰍠"

case "$(eww get menu_icon)" in 
    "$menu_icon_up")
    eww update menu_icon="$menu_icon_down"
    eww close menu
    ;;
    "$menu_icon_down")
    eww update menu_icon="$menu_icon_up" 
    eww open menu
    ;;
    *)
    ;;
esac

eww update
