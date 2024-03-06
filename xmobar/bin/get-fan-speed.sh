#!/bin/bash

# echo "\"$(sensors | grep "Right Side")\"" | \
#   jq --raw-output 'split(":")[1] | split(" ")[1]'

cat /sys/devices/platform/applesmc.768/fan1_input
