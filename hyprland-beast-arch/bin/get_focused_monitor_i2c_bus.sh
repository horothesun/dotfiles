#!/usr/bin/env bash

FOCUSED_MONITOR_SERIAL=$(
  hyprctl monitors all -j |\
    jq --raw-output 'map(select(.focused == true))[0] | .serial'
)

FOCUSED_MONITOR_I2C_BUS=$(
  ddcutil detect 2>/dev/null |\
    awk -v serial="$FOCUSED_MONITOR_SERIAL" '
      /I2C bus:/       { match($NF, /[0-9]+$/); last_bus = substr($NF, RSTART, RLENGTH) }
      /Serial number:/ && $NF == serial { print last_bus; exit }
    '
)

echo "$FOCUSED_MONITOR_I2C_BUS"
