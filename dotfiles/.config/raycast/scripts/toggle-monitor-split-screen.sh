#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Toggle Monitor Split Screen
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🖥️

# LG UltraFine DDC controls verified on this monitor:
# - VCP 0xD7: 1 disables PIP/PBP; 5 enables 50/50 PBP.

set -e

BD_CLI="/Applications/BetterDisplay.app/Contents/MacOS/BetterDisplay"
MONITOR="ULTRAFINE"
CONNECTION_MODE=$("$BD_CLI" get -nameLike="$MONITOR" -connectionMode)
SIGNAL_RESOLUTION=$(awk '{ print $3 }' <<< "$CONNECTION_MODE")

case "$SIGNAL_RESOLUTION" in
    2560x2160)
        MODE=1
        LABEL="off"
        ;;
    5120x2160)
        MODE=5
        LABEL="on"
        ;;
    *)
        echo "Unexpected signal resolution: $SIGNAL_RESOLUTION"
        exit 1
        ;;
esac

"$BD_CLI" set -nameLike="$MONITOR" -ddc="$MODE" -vcp=0xD7
echo "Turned split screen $LABEL"
