#!/bin/bash

INTERNAL_DISPLAY_NAME="eDP1"
HDMI_DISPLAY_NAME="HDMI2"

xrandr \
  --output "${INTERNAL_DISPLAY_NAME}" --primary --mode "1920x1200" --rate "60" --scale "1x1" --pos "960x2160" --rotate "normal" \
  --output "${HDMI_DISPLAY_NAME}" --mode "3840x2160" --rate "30" --scale "1x1" --pos "0x0" --rotate "normal" \
  --output "DP1" --off \
  --output "DP2" --off \
  --output "HDMI1" --off \
  --output "VIRTUAL1" --off
