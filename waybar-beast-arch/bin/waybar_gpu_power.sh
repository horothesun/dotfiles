#!/bin/bash

function get_gpu_power() {
    # NVIDIA via nvidia-smi (no sudo usually needed if driver installed)
    if command -v nvidia-smi >/dev/null 2>&1; then
        val=$(nvidia-smi --query-gpu=power.draw --format=csv,noheader,nounits 2>/dev/null | head -n1 || true)
        if [[ -n "${val:-}" ]]; then
            printf "%.2f" "$val"
            return
        fi
    fi

    # AMD / Intel GPU via hwmon
    for f in /sys/class/drm/card*/device/hwmon/hwmon*/power1_average \
             /sys/class/drm/card*/device/hwmon/hwmon*/power1_input; do
        if [[ -r "$f" ]]; then
            val=$(<"$f")
            awk "BEGIN { printf \"%.0f\", $val / 1000000 }"
            return
        fi
    done

    echo "⁉️"
}

echo "{\"text\":\"$(get_gpu_power)\"}"
