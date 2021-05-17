#!/bin/sh

if [ "$(mocp -Q %state)" != "STOP" ];then
    SONG=$(mocp -Q %song)
	ALBUM=$(mocp -Q %album)
        
    if [ -n "$SONG" ]; then
		echo "$(mocp -Q %artist): ${ALBUM:0:19} - ${SONG:0:20}"
    else
        basename "$(mocp -Q %file)"
    fi
else
    echo ""
fi
