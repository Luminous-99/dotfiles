if [ -z "$(pgrep conky)" ]; then
    conky
else
    pkill conky
fi
