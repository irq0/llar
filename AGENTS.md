# Agent instructions

Use these commands to test changes:

- Run Clojure unit tests with `lein unit`.
- On macOS, run database integration tests with `./tools/test-macos.sh`.
- On Linux with Docker available, run database integration tests with `lein integration`.
- After `npm ci`, run Node tests with `npm test`.
- Run browser tests with `npm run test:browser`.
- Run repository hooks on changed files with `prek run --files <paths>`.

Read [the test guide](doc/testing.md) for wrapper behavior and test selection.
Kaocha is the Clojure test runner.
To select tests, pass its arguments through the wrapper:
`./tools/test-macos.sh --focus llar.db.core-test`.

Use the wrapper to keep connection variables local to the test command.
Keep Ryuk enabled so that it can remove containers after failed tests.
Do not change global shell variables or Docker contexts for this setup.

The wrapper uses `TZ=UTC` to match CI.
A bookmark-capture timestamp test fails under Europe/Berlin with both Testcontainers 1.21.4 and 2.0.5.
Do not weaken its assertions to make a local test pass.
