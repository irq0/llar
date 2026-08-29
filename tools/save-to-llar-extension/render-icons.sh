#!/bin/sh
set -eu

cd "$(dirname "$0")"

for size in 32 48 128; do
  magick icons/icon-source.png \
    -resize "${size}x${size}" \
    -strip \
    "icons/icon-$size.png"
done

magick icons/icon-source-small.png \
  -resize 16x16 \
  -strip \
  icons/icon-16.png
