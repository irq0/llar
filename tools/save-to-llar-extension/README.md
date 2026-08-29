# 🖖 Save to LLAR browser extension

A small, store-free Chromium extension for sending the current page to LLAR's
durable bookmark capture queue. It navigates the current tab to LLAR's existing
capture feedback page, so the server remains responsible for authentication and
success reporting.

## Install in Arc or Chromium

1. Open `arc://extensions` in Arc or `chrome://extensions` in Chromium.
2. Enable **Developer mode**.
3. Select **Load unpacked** and choose this directory.
4. Pin **🖖 Save to LLAR** from the browser's extensions menu.
5. Right-click its icon, select **Options**, and enter the capture base URL and
   a dedicated browser token.

The capture base URL is the public root of LLAR's configured `:capture` service,
including any reverse-proxy path. The extension normalizes it to include the
trailing slash required by the relative capture routes.

Generate a token with `openssl rand -hex 32`, add it to the named credentials
entry used by `:api :capture`, and restart LLAR after changing the credentials.
The settings are stored locally in the browser profile and are cleared when the
extension is removed. They are not placed in this directory or synced by the
extension.

After editing extension files, use **Reload** on the extensions page.

The toolbar PNGs are rendered from two raster masters. `icons/icon-source.png`
provides the detailed 32, 48, and 128 pixel icons;
`icons/icon-source-small.png` uses the bolder background composition specifically
for the 16 pixel icon. With ImageMagick installed, regenerate every size with:

```sh
./render-icons.sh
```

The hand geometry is adapted from
[Emojitwo's `1f596.svg`](https://github.com/EmojiTwo/emojitwo/blob/master/svg/1f596.svg),
copyright Ranks.com and Emojitwo contributors, and used under
[CC-BY 4.0](https://creativecommons.org/licenses/by/4.0/). The adapted artwork
is included in the local raster masters; the extension does not load icon
assets from the web.
