#!/bin/bash

function to_GiB() {
  echo "scale = 1; $1 / (1024 * 1024 * 1024)" | bc
}

VRAM_USED=$(cat /sys/class/hwmon/hwmon1/device/mem_info_vram_used)
VRAM_TOTAL=$(cat /sys/class/hwmon/hwmon1/device/mem_info_vram_total)

VRAM_USED_GiB=$(to_GiB "${VRAM_USED}")
VRAM_TOTAL_GiB=$(to_GiB "${VRAM_TOTAL}")
VRAM_USED_PERCENT=$(echo "100 * ${VRAM_USED} / ${VRAM_TOTAL}" | bc)

echo "{
    \"text\": \"${VRAM_USED_PERCENT}\",
    \"tooltip\": \"${VRAM_USED_GiB} / ${VRAM_TOTAL_GiB} GiB\"
  }" |\
  jq --monochrome-output --unbuffered --compact-output
