#!/usr/bin/env bash

set -u

output="${1:-}"

if [[ -z "$output" ]]; then
    echo '{"text":"N/A","tooltip":"No monitor specified"}'
    exit 0
fi

# Get monitor information from Hyprland.
monitor=$(hyprctl monitors -j 2>/dev/null |
    jq -c --arg output "$output" '.[] | select(.name == $output)' |
    head -n1)

if [[ -z "$monitor" ]]; then
    jq -cn \
        --arg output "$output" \
        '{text: "N/A", tooltip: ($output + ": monitor not found")}'
    exit 0
fi

model=$(jq -r '.model // .description // .name' <<< "$monitor")
name=$(jq -r '.name' <<< "$monitor")

# ---------------------------------------------------------------------------
# Find the I2C bus corresponding to the Hyprland output.
# ---------------------------------------------------------------------------

bus=""
current_bus=""

while IFS= read -r line; do
    if [[ $line =~ I2C[[:space:]]+bus:[[:space:]]+/dev/i2c-([0-9]+) ]]; then
        current_bus="${BASH_REMATCH[1]}"
        continue
    fi

    if [[ $line =~ DRM_connector:[[:space:]]+(.+)$ ]]; then
        connector="${BASH_REMATCH[1]}"

        # Remove "cardN-" prefix.
        connector="${connector#*-}"

        if [[ "$connector" == "$output" ]]; then
            bus="$current_bus"
            break
        fi
    fi
done < <(ddcutil detect 2>/dev/null)

monitor_title="$model ($name)"

if [[ -z "$bus" ]]; then
    jq -cn \
        --arg title "$monitor_title" \
        '{text: "N/A", tooltip: ($title + ": DDC unavailable")}'
    exit 0
fi

# ---------------------------------------------------------------------------
# Get brightness (VCP 0x10).
# ---------------------------------------------------------------------------

brightness=$(
    ddcutil --skip-ddc-checks --bus "$bus" getvcp 10 2>/dev/null |
    sed -nE 's/.*current value = *([0-9]+).*/\1/p'
)

# ---------------------------------------------------------------------------
# Get contrast (VCP 0x12).
# ---------------------------------------------------------------------------

contrast=$(
    ddcutil --skip-ddc-checks --bus "$bus" getvcp 12 2>/dev/null |
    sed -nE 's/.*current value = *([0-9]+).*/\1/p'
)

if [[ -z "$brightness" ]]; then
    jq -cn \
        --arg title "$monitor_title" \
        --arg bus "$bus" \
        '{text: "N/A", tooltip: ($title + ": DDC unavailable (bus " + $bus + ")")}'
    exit 0
fi

tooltip="$monitor_title:   ${brightness}%"

if [[ -n "$contrast" ]]; then
    tooltip+="  󰆗  ${contrast}%"
else
    tooltip+="  󰆗  N/A"
fi

jq --null-input --compact-output --monochrome-output \
    --arg brightness "$brightness" \
    --arg tooltip "$(printf '%b' "$tooltip")" \
    '{text: $brightness, tooltip: $tooltip}'
