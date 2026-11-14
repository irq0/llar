import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { chromium } from "playwright";

test("dashboard previews bounded reprocessing, locks controls, and reports completion", async (t) => {
  const browser = await chromium.launch(
    process.env.LLAR_BROWSER_EXECUTABLE
      ? { executablePath: process.env.LLAR_BROWSER_EXECUTABLE, headless: true }
      : {
          channel: process.env.LLAR_BROWSER_CHANNEL ?? "chrome",
          headless: true,
        },
  );
  t.after(() => browser.close());
  const page = await browser.newPage();
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  let job = { status: "idle" };
  const writes = [];
  await page.route("http://llar.test/**", async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (url.pathname === "/api/reprocess/preview") {
      assert.equal(url.searchParams.get("source-key"), "newsletter");
      assert.equal(url.searchParams.get("days"), "30");
      assert.equal(url.searchParams.get("limit"), "100");
      return route.fulfill({
        json: { total: 100, limit: 100, "limited?": true },
      });
    }
    if (url.pathname === "/api/reprocess") {
      if (request.method() === "POST") {
        writes.push(
          Object.fromEntries(new URLSearchParams(request.postData())),
        );
        job = {
          "source-key": "newsletter",
          status: "running",
          completed: 0,
          total: 100,
        };
      }
      return route.fulfill({ json: job });
    }
    return route.fulfill({
      contentType: "text/html",
      body: `<form id="reprocess-form">
      <select id="reprocess-source" name="source-key"><option value="newsletter">Newsletter</option></select>
      <input id="reprocess-days" name="days" type="number" min="1" max="365" value="30" required>
      <input id="reprocess-limit" name="limit" type="number" min="1" max="500" value="100" required>
      <button type="submit">Preview</button>
      <button id="reprocess-start" type="button" disabled>Reapply</button>
      <p id="reprocess-status" role="status"></p></form>`,
    });
  });
  await page.goto("http://llar.test/");
  await page.addScriptTag({
    path: path.resolve("resources/status/jquery/jquery.min.js"),
  });
  await page.addScriptTag({
    path: path.resolve("resources/status/llar-status.js"),
  });
  await page.waitForResponse("http://llar.test/api/reprocess");
  assert.equal(await page.locator("#reprocess-start").isDisabled(), true);
  await page.getByText("Preview", { exact: true }).click();
  await page.waitForFunction(
    () => !document.querySelector("#reprocess-start").disabled,
  );
  assert.match(
    await page.locator("#reprocess-status").textContent(),
    /More match/,
  );
  assert.equal(writes.length, 0);
  await page.locator("#reprocess-days").fill("7");
  assert.equal(await page.locator("#reprocess-start").isDisabled(), true);
  await page.locator("#reprocess-days").fill("30");
  await page.getByText("Preview", { exact: true }).click();
  await page.locator("#reprocess-start").click();
  await page.waitForFunction(() =>
    document.querySelector("#reprocess-status").textContent.includes("Running"),
  );
  assert.equal(await page.locator("#reprocess-days").isDisabled(), true);
  assert.deepEqual(writes, [
    { "source-key": "newsletter", days: "30", limit: "100" },
  ]);
  job = { ...job, status: "complete", completed: 100 };
  await page.waitForFunction(() =>
    document
      .querySelector("#reprocess-status")
      .textContent.includes("Complete"),
  );
  assert.equal(await page.locator("#reprocess-days").isDisabled(), false);
  assert.equal(await page.locator("#reprocess-start").isDisabled(), true);
  assert.deepEqual(errors, []);
});
