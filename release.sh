#!/usr/bin/env bash
#
# Cut a new Extempore release.
#
# Usage: ./release.sh X.Y.Z[-suffix]
#
# The git tag is the single source of truth for the version (the binary derives
# it via `git describe`, and the release CI names artifacts after the tag), so
# this script bumps nothing in-tree --- it validates, then creates the annotated
# tag. Release notes stay hand-written: the matching "## vX.Y.Z" CHANGELOG.md
# section must already be committed.
#
# It deliberately does NOT push. Pushing the tag is what publishes the release
# (it triggers .github/workflows/release-binary.yml --- the Linux/macOS/Windows
# build matrix and the public GitHub release), so that stays a manual step.

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "usage: $0 X.Y.Z[-suffix]" >&2
    exit 1
fi

version="$1"
tag="v${version}"

if [[ ! "$version" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.]+)?$ ]]; then
    echo "error: '$version' is not a valid X.Y.Z[-suffix] version" >&2
    exit 1
fi

cd "$(git rev-parse --show-toplevel)"

branch="$(git rev-parse --abbrev-ref HEAD)"
if [[ "$branch" != "master" ]]; then
    echo "error: on branch '$branch', expected 'master'" >&2
    exit 1
fi

if ! git diff-index --quiet HEAD --; then
    echo "error: working tree is dirty --- commit or stash before releasing" >&2
    exit 1
fi

if git rev-parse -q --verify "refs/tags/${tag}" >/dev/null; then
    echo "error: tag ${tag} already exists" >&2
    exit 1
fi

if ! grep -qx "## ${tag}" CHANGELOG.md; then
    echo "error: CHANGELOG.md has no '## ${tag}' section --- add the release notes first" >&2
    exit 1
fi

echo "Tagging ${tag} at $(git rev-parse --short HEAD) ($(git log -1 --format=%s))"
git tag -a "${tag}" -m "Extempore ${tag}"

cat <<EOF

Created annotated tag ${tag}. Nothing has been pushed.

To publish (triggers the release build matrix and the public GitHub release):

    git push origin ${branch} ${tag}
EOF
