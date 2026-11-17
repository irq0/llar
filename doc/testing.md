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
Before you run browser tests, download the frontend assets with `make bootstrap jquery datatables ibmplex fontawesome`.
Install the browser engines with `npx playwright install chromium webkit`.
On Linux, add `--with-deps` to install its system libraries. CI installs them automatically.

## Local Reader integration tests

`npm run test:browser` runs both the isolated fixtures and the real Reader integration tests.
Use `npm run test:browser:integration` to run only the real Reader tests.
Use `npm run test:browser:fixtures` to run only the isolated fixtures in `tools/frontend/test`.

The real Reader tests require Java, Leiningen, and PostgreSQL server tools on `PATH`:
`initdb`, `pg_ctl`, and `createdb`. Run them as an ordinary user, not root.
On macOS, Homebrew PostgreSQL supplies these tools.
On Linux, install PostgreSQL and add the directory from `pg_config --bindir` to `PATH`.
The test runner starts PostgreSQL itself. It does not use an installed database service.

The harness creates a temporary PostgreSQL cluster and blob directory, then applies LLAR's migrations.
It starts the normal Reader handler from the current checkout on an automatically assigned loopback port.
Requests use the real routes, renderer, middleware, assets, and database operations.
The harness does not start fetch schedules, background jobs, or configuration watchers.
It never connects to a running personal Reader or database.

Before each scenario, the harness clears the test tables and restores 32 items from LLAR's demo generator.
The seed is `4242`, and the content reference time is `2026-11-17T12:00:00Z`.
Reset also clears runtime overrides and source update state.
Reset commands travel through the child process's input pipe. There is no reset HTTP endpoint.
The suite runs serially and creates a fresh browser context for every scenario.
Database processes and temporary storage are removed during teardown, including after failed assertions.

The fixed content dates support repeatable history tests. They do not freeze the application or database clock.
Tests of time-relative filters such as `today` need a separate clock fixture.
The current scenarios do not write blobs or start background jobs. Extend reset before adding those scenarios.

The suite covers Preview, Headlines, and Gallery in desktop Chromium, mobile Chromium, and mobile WebKit.
It asserts nonzero departure scroll, the original URL, and item position within eight pixels.
Additional cases cover ranked batches, fragment links, Inspect, separate visits, blocked storage, and database reset.
The tests do not replace page HTML or mock LLAR's API responses.
The focused artificial-page tests remain useful for delayed images and missing-item edge cases.
Chromium tests distinguish a new document from a cache restore through `pageshow.persisted`.
Separate cache tests require two successful cached returns. WebKit cache-hit tests are explicitly skipped.
Mobile explicit-refresh cases are marked `fixme`: the real mobile renderer has no visible refresh control.
The existing fixture suite includes an artificial mobile refresh link and cannot establish that this control exists in LLAR.

Failure screenshots and traces, plus Reader and PostgreSQL logs, go to `target/browser-integration`.
JUnit results go to `target/test-results/browser-integration.xml`.
CI runs this suite on pull requests and retains these artifacts.

## Reader history tests

This section describes the isolated fixture suite and the optional runner for an existing local Reader.

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

The isolated fixtures and optional live runner use installed Chrome by default.
The real Reader integration tests use Playwright's Chromium by default.
Set `LLAR_BROWSER_CHANNEL=chromium` to use Playwright's Chromium for both suites.
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
