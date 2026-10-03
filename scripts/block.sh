#!/usr/bin/env bash
set -euo pipefail

[ "$(id -u)" -eq 0 ] || { echo "Run as root." >&2; exit 1; }

blocklist=(
  "bbc.co.uk"
  "bbc.com"
  "news.ycombinator.com"
  "nitter.cf"
  "www.nitter.cf"
  "reddit.com"
  "www.reddit.com"
  "spurscommunity.co.uk"
  "www.spurscommunity.co.uk"
  "theguardian.com"
  "theguardian.co.uk"
  "www.theguardian.co.uk"
)

if [ "$(uname)" = Darwin ]; then
  chflags nouchg /etc/hosts 2>/dev/null || true
else
  chattr -i /etc/hosts 2>/dev/null || true
fi

tmp=$(mktemp /etc/hosts.XXXXXX)
trap 'rm -f "$tmp"' EXIT

cat > "$tmp" <<'EOF'
127.0.0.1       localhost
127.0.0.1       localhost.localdomain
127.0.0.1       local
255.255.255.255 broadcasthost
::1             localhost ip6-localhost ip6-loopback
EOF

for host in "${blocklist[@]}"; do
  printf '0.0.0.0 %s\n' "$host"
done >> "$tmp"

chmod 644 "$tmp"
if [ "$(uname)" = Darwin ]; then
  chown root:wheel "$tmp"
else
  chown root:root "$tmp"
fi

mv "$tmp" /etc/hosts

if [ "$(uname)" = Darwin ]; then
  chflags uchg /etc/hosts
else
  chattr +i /etc/hosts
fi

echo "Replaced /etc/hosts with ${#blocklist[@]} blocked hosts and set immutable."
