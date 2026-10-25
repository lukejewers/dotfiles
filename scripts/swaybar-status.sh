#!/usr/bin/env bash
# swaybar status_command: prints one status line every 5s, or immediately on SIGUSR1
set -uo pipefail

sleep_pid=
echo $$ > "${XDG_RUNTIME_DIR:-/tmp}/swaybar-status.pid"
trap 'kill "$sleep_pid" 2>/dev/null' USR1

read_cpu() { read -r _ user nice system idle iowait irq softirq steal _ < /proc/stat
             cpu_busy=$((user + nice + system + irq + softirq + steal))
             cpu_total=$((cpu_busy + idle + iowait)); }

net() {
  local dev ssid
  for dev in /sys/class/net/*; do
    [ -d "$dev/wireless" ] || continue
    ssid=$(iw dev "${dev##*/}" link 2>/dev/null | sed -n 's/^\s*SSID: //p')
    [ -n "$ssid" ] && { printf '󰖩 %s' "$ssid"; return; }
  done
  for dev in /sys/class/net/*; do
    [ "${dev##*/}" = lo ] && continue
    [ "$(cat "$dev/operstate" 2>/dev/null)" = up ] && { printf '󰈀 %s' "${dev##*/}"; return; }
  done
  printf '󰖪 down'
}

volume() {
  local out vol
  out=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null) || { printf '󰝟 n/a'; return; }
  vol=$(awk '{ printf "%d", $2 * 100 + 0.5 }' <<< "$out")
  [[ $out == *MUTED* ]] && printf '󰝟 %s%%' "$vol" || printf '󰕾 %s%%' "$vol"
}

memory() { awk '/^MemTotal/ { t = $2 } /^MemAvailable/ { a = $2 }
               END { printf "󰍛 %d%%", (t - a) * 100 / t }' /proc/meminfo; }

backlight() {
  local bl=(/sys/class/backlight/*)
  [ -e "${bl[0]}" ] || return
  printf '  󰃠 %d%%' $(( $(cat "${bl[0]}/brightness") * 100 / $(cat "${bl[0]}/max_brightness") ))
}

battery() {
  local bat=(/sys/class/power_supply/BAT*)
  [ -e "${bat[0]}" ] || return
  [ "$(cat "${bat[0]}/status")" = Discharging ] && printf '  󰁹 ' || printf '  󰂄 '
  printf '%s%%' "$(cat "${bat[0]}/capacity")"
}

read_cpu
while :; do
  prev_busy=$cpu_busy prev_total=$cpu_total
  read_cpu
  cpu=$(( (cpu_total - prev_total) > 0 ? (cpu_busy - prev_busy) * 100 / (cpu_total - prev_total) : 0 ))
  printf '%s  %s  󰻠 %d%%  %s%s%s  󰃭 %s \n' \
    "$(net)" "$(volume)" "$cpu" "$(memory)" "$(backlight)" "$(battery)" "$(date '+%a %d %b  󰥔 %H:%M')"
  sleep 5 & sleep_pid=$!
  wait "$sleep_pid"; sleep_pid=
done
