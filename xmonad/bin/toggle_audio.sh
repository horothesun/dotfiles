#!/bin/bash

AUDIO_CARD_ID="$1"
[[ -z "${AUDIO_CARD_ID}" ]] && echo "Error: AUDIO_CARD_ID must be passed as first argument" && exit 10

if [[ $(amixer --card "${AUDIO_CARD_ID}" get "Master" | grep "\[on\]") == "" ]]; then
  echo "Unmuting..."
  amixer --card "${AUDIO_CARD_ID}" sset "Bass Speaker" unmute
  amixer --card "${AUDIO_CARD_ID}" sset "Speaker"      unmute
  amixer --card "${AUDIO_CARD_ID}" sset "Headphone"    unmute
  amixer --card "${AUDIO_CARD_ID}" sset "Master"       unmute
else
  echo "Muting..."
  amixer --card "${AUDIO_CARD_ID}" sset "Master"       mute
  amixer --card "${AUDIO_CARD_ID}" sset "Headphone"    mute
  amixer --card "${AUDIO_CARD_ID}" sset "Speaker"      mute
  amixer --card "${AUDIO_CARD_ID}" sset "Bass Speaker" mute
fi
