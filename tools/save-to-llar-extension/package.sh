#!/bin/sh
set -eu

extension_dir=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)
repository_dir=$(CDPATH='' cd -- "$extension_dir/../.." && pwd)
version=$(awk -F '"' '/"version"[[:space:]]*:/ { print $4; exit }' \
  "$extension_dir/manifest.json")

if [ -z "$version" ]; then
  echo "Could not read the extension version from manifest.json" >&2
  exit 1
fi

output_dir=${1:-"$repository_dir/target/browser-extension"}
case "$output_dir" in
  /*) ;;
  *) output_dir="$repository_dir/$output_dir" ;;
esac

mkdir -p "$output_dir"
archive="$output_dir/save-to-llar-$version.zip"

cd "$extension_dir"
zip -q -FS "$archive" \
  manifest.json \
  background.mjs \
  extension-api.mjs \
  settings.mjs \
  options.html \
  options.mjs \
  PRIVACY.md \
  icons/icon-16.png \
  icons/icon-32.png \
  icons/icon-48.png \
  icons/icon-128.png

printf '%s\n' "$archive"
