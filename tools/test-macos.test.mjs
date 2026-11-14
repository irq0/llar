import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import os from "node:os";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

const wrapper = fileURLToPath(new URL("./test-macos.sh", import.meta.url));

function runWrapper(
  t,
  { running = false, testStatus = 0, bridgeStatus = 0 } = {},
) {
  const directory = mkdtempSync(path.join(os.tmpdir(), "llar-test-macos-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const commands = {
    uname: 'case "$1" in -s) echo Darwin;; -m) echo arm64;; esac',
    curl: '[ -f "$LLAR_TEST_DIRECTORY/ready" ]',
    container: 'printf "%s\\n" "$*" >> "$LLAR_TEST_DIRECTORY/pulls"',
    lein: `printf '%s\\n' "$@" > "$LLAR_TEST_DIRECTORY/args"
printf '%s\\n' "$TZ" "$DOCKER_HOST" "$TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE" "$TESTCONTAINERS_RYUK_DISABLED" > "$LLAR_TEST_DIRECTORY/env"
exit "$LLAR_TEST_STATUS"`,
    socktainer: `printf '%s\\n' "$@" > "$LLAR_TEST_DIRECTORY/started"
if [ "$LLAR_TEST_BRIDGE_STATUS" -ne 0 ]; then exit "$LLAR_TEST_BRIDGE_STATUS"; fi
trap 'touch "$LLAR_TEST_DIRECTORY/stopped"; rm -f "$LLAR_TEST_DIRECTORY/ready"; exit 0' TERM
touch "$LLAR_TEST_DIRECTORY/ready"
while :; do /bin/sleep 1; done`,
  };
  for (const [name, source] of Object.entries(commands)) {
    writeFileSync(
      path.join(directory, name),
      `#!/bin/sh\nset -eu\n${source}\n`,
      {
        mode: 0o755,
      },
    );
  }
  if (running) writeFileSync(path.join(directory, "ready"), "");
  const result = spawnSync(
    "/bin/sh",
    [wrapper, "--focus", "llar.db.core-test"],
    {
      cwd: os.tmpdir(),
      env: {
        ...process.env,
        PATH: `${directory}:${process.env.PATH}`,
        LLAR_TEST_DIRECTORY: directory,
        LLAR_TEST_STATUS: String(testStatus),
        LLAR_TEST_BRIDGE_STATUS: String(bridgeStatus),
        TESTCONTAINERS_RYUK_DISABLED: "true",
        TZ: "Europe/Berlin",
      },
      encoding: "utf8",
      timeout: 15000,
    },
  );
  assert.ifError(result.error);
  return { directory, result };
}

test("macOS wrapper scopes the environment, forwards arguments, and stops its bridge", (t) => {
  const { directory, result } = runWrapper(t);
  assert.equal(result.status, 0, result.stderr);
  assert.equal(
    readFileSync(path.join(directory, "started"), "utf8"),
    "--no-docker-context\n",
  );
  assert.equal(
    readFileSync(path.join(directory, "args"), "utf8"),
    "integration\n--focus\nllar.db.core-test\n",
  );
  const socket = `${process.env.HOME}/.socktainer/container.sock`;
  assert.equal(
    readFileSync(path.join(directory, "env"), "utf8"),
    `UTC\nunix://${socket}\n${socket}\nfalse\n`,
  );
  assert.equal(
    readFileSync(path.join(directory, "pulls"), "utf8"),
    "image pull --platform linux/arm64 docker.io/testcontainers/ryuk:0.14.0\nimage pull --platform linux/arm64 docker.io/library/postgres:16\n",
  );
  assert.ok(existsSync(path.join(directory, "stopped")));
});

test("macOS wrapper preserves test failures and still stops its bridge", (t) => {
  const { directory, result } = runWrapper(t, { testStatus: 7 });
  assert.equal(result.status, 7, result.stderr);
  assert.ok(existsSync(path.join(directory, "stopped")));
});

test("macOS wrapper reuses an existing bridge without stopping it", (t) => {
  const { directory, result } = runWrapper(t, { running: true });
  assert.equal(result.status, 0, result.stderr);
  assert.ok(!existsSync(path.join(directory, "started")));
  assert.ok(!existsSync(path.join(directory, "stopped")));
  assert.ok(existsSync(path.join(directory, "ready")));
});

test("macOS wrapper reports a failed bridge before running tests", (t) => {
  const { directory, result } = runWrapper(t, { bridgeStatus: 9 });
  assert.equal(result.status, 1);
  assert.match(result.stderr, /Socktainer did not start/);
  assert.ok(!existsSync(path.join(directory, "args")));
});
