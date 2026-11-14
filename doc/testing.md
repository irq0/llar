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
