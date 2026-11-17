import assert from "node:assert/strict";
import { spawn, execFileSync } from "node:child_process";
import { once } from "node:events";
import { createWriteStream, existsSync } from "node:fs";
import { copyFile, mkdir, mkdtemp, rm } from "node:fs/promises";
import net from "node:net";
import os from "node:os";
import path from "node:path";
import { createInterface } from "node:readline";

// One owned PostgreSQL cluster and one Reader, used serially by this suite.
export async function startReader(logName = "server.log") {
  await mkdir("target/browser-integration", { recursive: true });
  const directory = await mkdtemp(path.join(os.tmpdir(), "llar-browser-"));
  const data = path.join(directory, "postgres");
  const logPath = path.join("target/browser-integration", logName);
  const log = createWriteStream(logPath);
  const env = { ...process.env, TZ: "UTC" };
  let child;
  const run = (command, args) => {
    try {
      return execFileSync(command, args, {
        env,
        encoding: "utf8",
        timeout: 60000,
      });
    } catch (error) {
      throw new Error(`${command} failed: ${error.stderr || error.message}`, {
        cause: error,
      });
    }
  };
  async function stop() {
    if (child?.pid && child.exitCode === null && child.signalCode === null) {
      const exited = once(child, "exit");
      child.stdin.end();
      const timer = setTimeout(() => child.kill("SIGTERM"), 10000);
      const killTimer = setTimeout(() => child.kill("SIGKILL"), 15000);
      try {
        await exited;
      } finally {
        clearTimeout(timer);
        clearTimeout(killTimer);
      }
    }
    try {
      if (existsSync(path.join(data, "postmaster.pid")))
        run("pg_ctl", ["-D", data, "-m", "immediate", "-w", "stop"]);
    } finally {
      if (existsSync(path.join(directory, "postgres.log")))
        await copyFile(
          path.join(directory, "postgres.log"),
          path.join("target/browser-integration", `postgres-${logName}`),
        );
      log.end();
      await rm(directory, { recursive: true, force: true });
    }
  }
  try {
    run("initdb", [
      "-D",
      data,
      "-U",
      "postgres",
      "--auth=trust",
      "--no-locale",
      "-E",
      "UTF8",
    ]);
    const reservation = net.createServer();
    reservation.listen(0, "127.0.0.1");
    await once(reservation, "listening");
    const port = reservation.address().port;
    await new Promise((resolve) => reservation.close(resolve));
    run("pg_ctl", [
      "-D",
      data,
      "-l",
      path.join(directory, "postgres.log"),
      "-w",
      "start",
      "-o",
      `-h 127.0.0.1 -p ${port} -k ${directory}`,
    ]);
    run("createdb", [
      "-h",
      "127.0.0.1",
      "-p",
      String(port),
      "-U",
      "postgres",
      "llar_browser_test",
    ]);
    const blobs = path.join(directory, "blobs");
    await mkdir(blobs);
    child = spawn(
      "lein",
      [
        "with-profile",
        "+test",
        "trampoline",
        "run",
        "-m",
        "llar.browser-test-server",
        String(port),
        blobs,
      ],
      { env, stdio: ["pipe", "pipe", "pipe"] },
    );
    child.stdout.pipe(log, { end: false });
    child.stderr.pipe(log, { end: false });
    const lines = createInterface({ input: child.stdout });
    let pending;
    let failed;
    function fail(error) {
      failed = error;
      pending?.reject(error);
      pending = undefined;
    }
    child.on("error", fail);
    child.stdin.on("error", fail);
    child.on("exit", (code, signal) =>
      fail(new Error(`Reader exited (${code ?? signal}); see ${logPath}`)),
    );
    lines.on("line", (line) => {
      if (!line.startsWith("LLAR_BROWSER_TEST ")) return;
      const result = JSON.parse(line.slice("LLAR_BROWSER_TEST ".length));
      if (result.error) pending?.reject(new Error(result.error));
      else pending?.resolve(result);
      pending = undefined;
    });
    function receive(command) {
      if (failed) return Promise.reject(failed);
      assert.equal(pending, undefined, "Reader commands must run serially");
      return new Promise((resolve, reject) => {
        const timer = setTimeout(() => {
          fail(new Error(`Reader timed out; see ${logPath}`));
        }, 120000);
        pending = {
          resolve: (value) => {
            clearTimeout(timer);
            resolve(value);
          },
          reject: (error) => {
            clearTimeout(timer);
            reject(error);
          },
        };
        if (command) child.stdin.write(JSON.stringify({ command }) + "\n");
      });
    }
    const { url } = await receive();
    return { url, reset: () => receive("reset"), stop };
  } catch (error) {
    await stop();
    throw error;
  }
}
