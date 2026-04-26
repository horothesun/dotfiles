#!/bin/bash

# Most Ryzens expose package power in powercap
# IMPORTANT: make the following file readable with
# sudo chmod -R a+r /sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj
CPU_PATH="/sys/class/powercap/intel-rapl/intel-rapl:0/energy_uj"

if [ -f "$CPU_PATH" ]; then
    # RAPL provides energy in Joules; we measure the delta over 0.3s
    E1=$(cat "$CPU_PATH")
    sleep 0.3
    E2=$(cat "$CPU_PATH")
    # Calculation: (E2 - E1) / 300,000 = Watts
    CPU_W=$(awk "BEGIN { printf \"%.0f\", ($E2 - $E1) / 300000 }")
else
    CPU_W="⁉️"
fi

echo "{\"text\":\"${CPU_W}\"}"
