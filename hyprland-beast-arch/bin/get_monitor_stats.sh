#!/usr/bin/env bash

set -euo pipefail

MONITOR_I2C_BUS="${1:?Usage: $(basename "$0") <i2c_bus>}"

ddcutil --bus "$MONITOR_I2C_BUS" getvcp 10 12 |\
  jq --raw-input --raw-output '
      split(" ")
    | map(select(. != ""))
    | {
        "\(.[3] | split("(")[1] | ascii_downcase)": {
          "value": .[8] | split(",")[0] | tonumber,
          "max": .[12] | tonumber
          }
      }' |\
  jq --slurp 'add'
