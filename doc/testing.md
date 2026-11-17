# Test guide

LLAR is covered by:

- unit tests - clojure
- integration - mostly clojure, using testcontainers and a real database
- node tests, wrapper tests - cover node tools we shell out to like article extraction
- browser tests - covers the reader and dashboard interface

Use these commands from the repository root:

| Tests | Command |
| --- | --- |
| Clojure unit tests | `lein unit` |
| Database tests on Linux with Docker | `lein integration` |
| Database tests on macOS | `./tools/test-macos.sh` |
| Node tests and wrapper tests | `npm test` |
| Browser tests | `npm run test:browser` |

Before you run Node or browser tests, install the project dependencies with `npm ci`.
Before you run browser tests, download the frontend assets with `make bootstrap jquery datatables`.
The browser tests use the files in `tools/frontend/test`.
Install WebKit for the mobile history tests with `npx playwright install webkit`.
On Linux, add `--with-deps` to install its system libraries. CI installs them automatically.

## Reader history tests

The same history scenarios run in desktop Chromium, mobile Chromium, and mobile WebKit.
WebKit covers Safari's browser engine, but it does not replace testing on a physical iPhone or iPad.
Mobile cases use touch taps and the Bootstrap navigation drawer with Reader's actual CSS and JavaScript.
The local pages send LLAR's `Referrer-Policy: no-referrer` header.

Run only this suite with:

```sh
node --test tools/frontend/test/reader-history.test.mjs
```

The scenarios cover Back, Forward, Reload, list links, separate visits, filters, ranked batches,
missing items, delayed images, Skip to content, Inspect item, and blocked session storage.
They measure item position within eight pixels and require the original URL, including its batch.
The tests wait for scrolling to stop before measuring. They do not accept a different position to make a test pass.

For a running local Reader, use:

```sh
LLAR_READER_URL=http://127.0.0.1:8023 node tools/frontend/reader-history-live.mjs
```

This command opens already-read items in Preview, Headlines, and Gallery across the three browser profiles.
Normal Reader activity still applies. Reports and screenshots go to `target/reader-history`.
A list that cannot scroll is reported as untested and makes the command fail.

Chromium runs use installed Chrome by default.
For Playwright's Chromium, install it with `npx playwright install chromium` and set `LLAR_BROWSER_CHANNEL=chromium`.
If browsers use a custom installation directory, set `PLAYWRIGHT_BROWSERS_PATH` to that directory
for both installation and testing.

## macOS test wrapper

The wrapper runs the database tests through Socktainer and Apple Container.
Kaocha is the Clojure test runner.
The wrapper passes all arguments to Kaocha through `lein integration`.
To select one test namespace, use `--focus`:

```sh
./tools/test-macos.sh --focus llar.db.core-test
```

The wrapper starts Socktainer if it is not running.
If Socktainer is already running, the wrapper uses it.
The wrapper stops only the Socktainer process that it started, even when tests fail.
Read `target/socktainer.log` for output from that process.

Run one wrapper per user at a time.
All runs for that user share the same connection.
The wrapper keeps connection variables local to its command.
It does not change shell profiles or Docker contexts.

## Timestamp test issue

The wrapper uses `TZ=UTC` to match CI.
