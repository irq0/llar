# Demo screenshots

The screenshot harness captures the deterministic Compose demo for the project
README and GitHub Pages. From a running demo stack with Google Chrome installed,
run the capture:

```sh
npm run demo:screenshots
```

The Reader defaults to `http://127.0.0.1:8023`. Override it with
`LLAR_READER_URL`; override the output directory with `LLAR_SCREENSHOT_OUT`.
The browser channel defaults to `chrome` and can be changed with
`LLAR_BROWSER_CHANNEL`.
The published capture is `reader.png`. Traces and failure screenshots are kept
under `diagnostics/` and are never published to Pages.
