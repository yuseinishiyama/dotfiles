#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Set Monitor Input
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🖥️
# @raycast.argument1 { "type": "dropdown", "placeholder": "Input", "data": [{ "title": "USB-C", "value": "210" }, { "title": "DisplayPort", "value": "208" }] }

BD_CLI="/Applications/BetterDisplay.app/Contents/MacOS/BetterDisplay"
MONITOR="ULTRAFINE"
INPUT="$1"

case "$INPUT" in
    210) LABEL="USB-C" ;;
    208) LABEL="DisplayPort" ;;
    *) echo "Unknown input: $INPUT"; exit 1 ;;
esac

$BD_CLI set -nameLike="$MONITOR" -ddcAlt="$INPUT" -vcp=inputSelectAlt
echo "Set input to $LABEL"
