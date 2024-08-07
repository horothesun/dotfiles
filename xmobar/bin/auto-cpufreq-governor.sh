#!/bin/bash

( timeout 3s auto-cpufreq --stats | grep --line-buffered --max-count=1 "governor" ) 2>/dev/null | \
  xargs -I ^ echo "\"^\"" | \
  jq --raw-output '
      split(" ")[-2]
    | if (. == "powersave") then "󱙷"
      else
        if (. == "performance") then "󱐋" else "???" end
      end
  '
