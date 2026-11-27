#!/bin/sh
# Install Extempore for the current user, on macOS or Linux:
#
#   curl -fsSL https://extemporelang.github.io/install.sh | sh
#
# This downloads the latest release for this machine, checks it against the
# release's SHA256SUMS and unpacks it into ~/.local/share/extempore/VERSION.
# ~/.local/share/extempore/current then points at that version, and
# ~/.local/bin/extempore at current/bin/extempore. If ~/.local/bin isn't on
# your PATH, a line in your shell's startup file adds it. Run it again to
# upgrade; earlier versions stay where they are until you delete them.
#
# Environment variables:
#   EXTEMPORE_VERSION         install this release (e.g. v0.13.0), not the latest
#   EXTEMPORE_NO_MODIFY_PATH  set to leave your shell's startup files alone
#   XDG_DATA_HOME             instead of ~/.local/share
#   XDG_BIN_HOME              instead of ~/.local/bin
#   EXTEMPORE_DOWNLOAD_URL    where the archives and SHA256SUMS are (for testing)

set -eu

say() {
    printf '%s\n' "$*" >&2
}

die() {
    say "extempore install: $*"
    exit 1
}

need() {
    command -v "$1" >/dev/null 2>&1 || die "this needs $1, and it isn't installed"
}

fetch() {
    if [ -t 2 ]; then
        curl -fL --retry 3 --progress-bar -o "$2" "$1"
    else
        curl -fsSL --retry 3 -o "$2" "$1"
    fi || die "couldn't download $1"
}

sha256() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | cut -d ' ' -f 1
    else
        shasum -a 256 "$1" | cut -d ' ' -f 1
    fi
}

unpack() {
    if command -v unzip >/dev/null 2>&1; then
        unzip -q "$1" -d "$2"
    elif command -v bsdtar >/dev/null 2>&1; then
        mkdir -p "$2" && bsdtar -xf "$1" -C "$2"
    elif command -v python3 >/dev/null 2>&1; then
        python3 -c 'import sys, zipfile; zipfile.ZipFile(sys.argv[1]).extractall(sys.argv[2])' "$1" "$2"
    else
        die "this needs unzip (or bsdtar, or python3) to unpack the archive"
    fi
}

# The startup file of the user's shell, and the line which puts $1 on PATH.
add_to_path() {
    bindir=$1
    case "$bindir" in
    "$HOME"/*) bindir="\$HOME/${bindir#"$HOME"/}" ;;
    esac
    case "$(basename "${SHELL:-sh}")" in
    zsh)
        rc="${ZDOTDIR:-$HOME}/.zshrc"
        line="export PATH=\"$bindir:\$PATH\""
        ;;
    bash)
        if [ "$(uname -s)" = Darwin ]; then rc="$HOME/.bash_profile"; else rc="$HOME/.bashrc"; fi
        line="export PATH=\"$bindir:\$PATH\""
        ;;
    fish)
        rc="$HOME/.config/fish/conf.d/extempore.fish"
        line="fish_add_path \"$bindir\""
        ;;
    *)
        rc="$HOME/.profile"
        line="export PATH=\"$bindir:\$PATH\""
        ;;
    esac
    if ! grep -qsF "$line" "$rc"; then
        mkdir -p "$(dirname "$rc")"
        printf '\n# added by the Extempore installer\n%s\n' "$line" >>"$rc"
    fi
    say "Added $bindir to your PATH in $rc. Open a new terminal to use extempore there."
}

main() {
    need curl
    need uname

    case "$(uname -s)" in
    Linux) os=linux ;;
    Darwin) os=macos ;;
    *) die "this script is for macOS and Linux: on Windows, use install.ps1 (see the docs)" ;;
    esac
    case "$(uname -m)" in
    x86_64 | amd64) arch=x86_64 ;;
    arm64 | aarch64) arch=aarch64 ;;
    *) die "there's no Extempore release for $(uname -m)" ;;
    esac
    # a shell running under Rosetta reports x86_64 on Apple Silicon
    if [ "$os" = macos ] && [ "$arch" = x86_64 ]; then
        if [ "$(sysctl -n sysctl.proc_translated 2>/dev/null || true)" = 1 ]; then
            arch=aarch64
        else
            die "Extempore's macOS releases are for Apple Silicon only"
        fi
    fi

    releases="https://github.com/digego/extempore/releases"
    if [ -n "${EXTEMPORE_DOWNLOAD_URL:-}" ]; then
        base=$EXTEMPORE_DOWNLOAD_URL
    elif [ -n "${EXTEMPORE_VERSION:-}" ]; then
        base="$releases/download/$EXTEMPORE_VERSION"
    else
        base="$releases/latest/download"
    fi

    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT

    fetch "$base/SHA256SUMS" "$tmp/SHA256SUMS"
    line=$(grep " \*\{0,1\}extempore-$os-$arch-.*\.zip\$" "$tmp/SHA256SUMS" | head -n 1 || true)
    [ -n "$line" ] || die "this release has no download for $os $arch"
    sum=${line%% *}
    archive=${line##* }
    archive=${archive#\*}
    version=${archive#extempore-"$os"-"$arch"-}
    version=${version%.zip}

    say "Downloading Extempore $version for $os $arch"
    fetch "$base/$archive" "$tmp/$archive"
    [ "$(sha256 "$tmp/$archive")" = "$sum" ] || die "$archive doesn't match its checksum in SHA256SUMS"
    unpack "$tmp/$archive" "$tmp/unpacked"
    tree="$tmp/unpacked/${archive%.zip}"
    [ -f "$tree/bin/extempore" ] || die "$archive doesn't hold bin/extempore"
    chmod +x "$tree/bin/extempore"

    data="${XDG_DATA_HOME:-$HOME/.local/share}/extempore"
    bindir="${XDG_BIN_HOME:-$HOME/.local/bin}"
    mkdir -p "$data" "$bindir"
    rm -rf "$data/$version"
    mv "$tree" "$data/$version"
    ln -sfn "$version" "$data/current"
    ln -sf "$data/current/bin/extempore" "$bindir/extempore"

    "$bindir/extempore" --version >/dev/null 2>&1 || die "the installed $bindir/extempore doesn't run"
    say "Installed Extempore $version in $data/$version"

    case ":$PATH:" in
    *":$bindir:"*) ;;
    *)
        if [ -z "${EXTEMPORE_NO_MODIFY_PATH:-}" ]; then
            add_to_path "$bindir"
        else
            say "$bindir isn't on your PATH: add it, or run $bindir/extempore"
        fi
        ;;
    esac
    say "Run extempore to start it."
}

main "$@"
