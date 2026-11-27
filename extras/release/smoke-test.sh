#!/usr/bin/env bash
# Install a release archive with docs/public/install.sh, as a user would, and
# check the installed extempore runs from outside the tree. The archive and its
# SHA256SUMS are served from DIST over HTTP, into a throwaway HOME.
#
# Usage: extras/release/smoke-test.sh DIST

set -euo pipefail

dist=$(cd "$1" && pwd)
repo=$(cd "$(dirname "$0")/../.." && pwd)
scratch=$(mktemp -d)
trap 'kill "$server" 2>/dev/null || true; rm -rf "$scratch"' EXIT

port=8765
python3 -m http.server --bind 127.0.0.1 --directory "$dist" "$port" >/dev/null 2>&1 &
server=$!
for _ in $(seq 50); do
    curl -fsS -o /dev/null "http://127.0.0.1:$port/SHA256SUMS" && break
    sleep 0.2
done

mkdir -p "$scratch/home" "$scratch/work"
env -i HOME="$scratch/home" PATH=/usr/bin:/bin SHELL=/bin/bash \
    EXTEMPORE_DOWNLOAD_URL="http://127.0.0.1:$port" \
    sh "$repo/docs/public/install.sh"

extempore="$scratch/home/.local/bin/extempore"
"$extempore" --version
cd "$scratch/work"
out=$(env -i HOME="$scratch/home" PATH=/usr/bin:/bin "$extempore" exec -e \
    '(sys:load "libs/core/math.xtm") (println (sys:share-dir)) (println (sqrt 2.0))')
echo "$out"
grep -q "/.local/share/extempore/.*/share/extempore" <<<"$out"
grep -q "1.414214" <<<"$out"
# the installer added ~/.local/bin to PATH in bash's startup file
grep -q '.local/bin' "$scratch/home/.bashrc" "$scratch/home/.bash_profile" 2>/dev/null
echo "smoke test passed"
