import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { chromium } from "playwright";

test("source details scope previews, lock all forms, and recover running progress", async (t) => {
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
  let markReadWrites = 0;
  await page.route("http://llar.test/**", async (route) => {
    const request = route.request();
    const url = new URL(request.url());
    if (url.pathname === "/api/source/newsletter/read") {
      assert.equal(request.method(), "POST");
      markReadWrites += 1;
      return route.fulfill({
        json: { "affected-count": 12, "unread-count": 0 },
      });
    }
    if (url.pathname === "/api/sources") {
      return route.fulfill({
        json: {
          data: ["newsletter", "other"].map((key) => [
            key,
            "ok",
            key,
            "now",
            "now",
            "100 items",
          ]),
        },
      });
    }
    if (url.pathname.startsWith("/source-details/")) {
      const key = url.pathname.split("/").pop();
      return route.fulfill({
        contentType: "text/html",
        body: `<details class="source-reprocess"><summary>Reapply rules to stored items</summary><form class="reprocess-form">
          <input type="hidden" name="source-key" value="${key}">
          <label>Last days<input name="days" type="number" min="1" max="365" value="30" required></label>
          <label>Maximum items<input name="limit" type="number" min="1" max="500" value="100" required></label>
          <button type="submit">Preview</button>
          <button class="reprocess-start" type="button" disabled>Reapply</button>
          <p class="reprocess-status" role="status"></p></form></details>`,
      });
    }
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
      body: `<table id="sources-datatable"><thead><tr>
        <th>Source Key</th><th>Status</th><th>Source</th><th>Last Success</th>
        <th>Last Attempt</th><th>Stats</th><th>Actions</th>
        </tr></thead></table>`,
    });
  });
  await page.goto("http://llar.test/");
  await page.addStyleTag({
    path: path.resolve("resources/status/bootstrap/css/bootstrap.min.css"),
  });
  for (const script of [
    "jquery/jquery.min.js",
    "bootstrap/js/bootstrap.bundle.min.js",
    "datatables/dataTables.min.js",
  ]) {
    await page.addScriptTag({ path: path.resolve("resources/status", script) });
  }
  await page.evaluate(() => {
    window.llarValueInspector = { mountAll() {} };
  });
  await page.addScriptTag({
    path: path.resolve("resources/status/llar-status.js"),
  });
  assert.equal(await page.locator(".reprocess-form").count(), 0);
  const toggle = (key) =>
    page.locator(`.btn-source-details[data-source-key="${key}"]`);
  const menu = (key) =>
    toggle(key)
      .locator("xpath=ancestor::tr")
      .getByRole("button", { name: "Source actions" });
  const reapply = async () => {
    await menu("newsletter").click();
    await page
      .locator('.btn-source-reapply[data-source-key="newsletter"]')
      .click();
    assert.equal(
      await menu("newsletter").getAttribute("aria-expanded"),
      "false",
    );
  };
  const form = (key) =>
    page.locator(
      `.reprocess-form:has(input[name="source-key"][value="${key}"])`,
    );
  for (const key of ["other", "newsletter"]) {
    await Promise.all([
      page.waitForResponse("http://llar.test/api/reprocess"),
      key === "newsletter" ? reapply() : toggle(key).click(),
    ]);
  }
  assert.equal(
    await page
      .locator('.source-reprocess:has(input[value="other"])')
      .getAttribute("open"),
    null,
  );
  const newsletter = form("newsletter");
  const other = form("other");
  await other.locator("xpath=..").locator("summary").click();
  assert.equal(
    await newsletter.getByRole("button", { name: "Reapply" }).isDisabled(),
    true,
  );
  await newsletter.getByRole("button", { name: "Preview" }).click();
  await page.waitForFunction(
    () =>
      !document.querySelector(
        ".reprocess-form:has(input[value='newsletter']) .reprocess-start",
      ).disabled,
  );
  assert.match(
    await newsletter.getByRole("status").textContent(),
    /More match/,
  );
  assert.equal(
    await other.getByRole("button", { name: "Reapply" }).isDisabled(),
    true,
  );
  assert.equal(await other.getByRole("status").textContent(), "");
  assert.equal(writes.length, 0);

  await newsletter.getByLabel("Last days").fill("7");
  assert.equal(
    await newsletter.getByRole("button", { name: "Reapply" }).isDisabled(),
    true,
  );
  await newsletter.getByLabel("Last days").fill("30");
  await newsletter.getByRole("button", { name: "Preview" }).click();
  await newsletter.getByRole("button", { name: "Reapply" }).click();
  await newsletter.getByText(/Running/).waitFor();
  assert.equal(await newsletter.getByLabel("Last days").isDisabled(), true);
  assert.equal(
    await other.getByRole("button", { name: "Preview" }).isDisabled(),
    true,
  );
  assert.match(
    await other.getByRole("status").textContent(),
    /Waiting for newsletter/,
  );
  assert.deepEqual(writes, [
    { "source-key": "newsletter", days: "30", limit: "100" },
  ]);

  // Reopening the row must resume progress and keep the controls locked.
  await toggle("newsletter").click();
  await reapply();
  await newsletter.getByText(/Running/).waitFor();
  assert.equal(await newsletter.getByLabel("Last days").isDisabled(), true);
  job = { ...job, status: "complete", completed: 100 };
  await newsletter.getByText(/Complete/).waitFor();
  assert.equal(await newsletter.getByLabel("Last days").isDisabled(), false);
  assert.equal(
    await newsletter.getByRole("button", { name: "Reapply" }).isDisabled(),
    true,
  );
  assert.equal(
    await other.getByRole("button", { name: "Preview" }).isDisabled(),
    false,
  );
  assert.equal(await other.getByRole("status").textContent(), "");
  const markRead = page.locator(
    '.btn-mark-source-read[data-source-key="newsletter"]',
  );
  await menu("newsletter").click();
  assert.equal(
    await page.locator(".dropdown-menu.show .btn-update-source").count(),
    3,
  );
  page.once("dialog", (dialog) => dialog.dismiss());
  await markRead.click();
  assert.equal(markReadWrites, 0);
  await menu("newsletter").click();
  page.once("dialog", (dialog) => {
    assert.match(dialog.message(), /Mark all unread items from newsletter/);
    return dialog.accept();
  });
  await markRead.click();
  await page.getByText("Marked 12 items read.", { exact: true }).waitFor();
  assert.equal(markReadWrites, 1);
  assert.equal(await markRead.isDisabled(), true);
  assert.deepEqual(errors, []);
});
