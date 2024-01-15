#!/bin/bash

MY_CARD_ID=1

if [[ $(amixer --card "${MY_CARD_ID}" get "Master" | grep "\[on\]") == "" ]]; then
  echo "Unmuting..."
  amixer --card "${MY_CARD_ID}" sset "Bass Speaker" unmute
  amixer --card "${MY_CARD_ID}" sset "Speaker"      unmute
  amixer --card "${MY_CARD_ID}" sset "Headphone"    unmute
  amixer --card "${MY_CARD_ID}" sset "Master"       unmute
else
  echo "Muting..."
  amixer --card "${MY_CARD_ID}" sset "Master"       mute
  amixer --card "${MY_CARD_ID}" sset "Headphone"    mute
  amixer --card "${MY_CARD_ID}" sset "Speaker"      mute
  amixer --card "${MY_CARD_ID}" sset "Bass Speaker" mute
fi
