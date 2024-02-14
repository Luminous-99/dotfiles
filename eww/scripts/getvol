#!/bin/sh

amixer -D pulse sget Master | awk -F '[^0-9]+' '/Left:/{print $3}'
