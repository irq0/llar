# Agent instructions

## Review workflow

Keep changes in small parts that the user can review separately.
Preserve unrelated work and existing staged changes.
If the user requests staged review, stage only the agreed part.
Suggest a commit message for that part.
Wait for the user to review it before moving to the next part.

## Tests and hooks

Use these commands to test changes:

- Run Clojure unit tests with `lein unit`.
- For Clojure changes, run `clj-kondo --lint src:test:.clj-kondo/hooks`.
- On macOS, run database integration tests with `./tools/test-macos.sh`.
- On Linux with Docker available, run database integration tests with `lein integration`.
- After `npm ci`, run Node tests with `npm test`.
- Run browser tests with `npm run test:browser`.
- Before handing changes back, run `prek run --all-files`.

Fix failures caused by your changes.
Rerun the failed commands after fixes.
Report any remaining failures or commands that you cannot run.
Prek does not run clj-kondo.

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

## SQL, extraction, and reprocessing

If you add HugSQL functions, update their declarations in `src/llar/db/sql.clj`.
HugSQL generates these functions from SQL files.
The declarations make the functions visible to clj-kondo.

## General Guidelines

Prefer source configuration and the existing article extractor before adding code for a specific source.
If HTML is already available, extract from it without fetching it again.
Test selectors against real HTML after LLAR sanitizes it.
Make sure that repeated links produce one item and that page layout changes do not change item identity.
Report which parts you tested: link selection, article extraction, and metadata.

## Documentation

Keep the README focused on getting started.
Put detailed behavior in the relevant documentation.
Record changes that affect users in the changelog.
Keep `AGENTS.md` short and actionable.
Link to detailed policies instead of copying them into `AGENTS.md`.
