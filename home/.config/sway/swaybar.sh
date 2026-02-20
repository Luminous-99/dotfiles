#!/bin/bash

audio=$(amixer sget Master  | awk -F"[][]" '/dB/ {print $2}')

datetime=$(date +'%Y-%m-%d %I:%M:%S %p')

battery_charge=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "percentage" | awk '{print $2}')

battery_status=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "state" | awk '{print $2}')

if [ $battery_status = "discharging" ];
then
    battery_pluggedin='⚠'
else
    battery_pluggedin='⚡'
fi


echo "| 🎝 $audio | $battery_pluggedin $battery_charge | $datetime"


