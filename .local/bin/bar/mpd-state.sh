#!/bin/sh

if [ "$(pgrep ncmpcpp)" == "" ]; then
    echo ""
elif [ "$(mpc | grep playing)" != "" ]; then
    echo "" `mpc current -f "%artist%  %title%"`
elif [ "$(mpc | grep paused)" != "" ]; then
    echo "" `mpc current -f "%artist%  %title%"`
else
    echo ""
fi
