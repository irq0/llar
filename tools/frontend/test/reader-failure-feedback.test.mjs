import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

import { chromium } from "playwright";

const repositoryRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);
const asset = (...parts) =>
  path.join(repositoryRoot, "resources/status", ...parts);
const browserChannel = process.env.LLAR_BROWSER_CHANNEL ?? "chrome";

test("Failed item actions stay put and expose a persistent error", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage({ viewport: { width: 320, height: 700 } });
  await page.setContent(`
    <!doctype html>
    <html>
      <head><meta name="llar-mode" content="list-items"></head>
      <body class="llar-reader">
        <div id="reader-global-status"
             class="reader-global-status"
             role="alert"
             aria-live="assertive"
             aria-atomic="true"
             hidden>
          <span class="reader-global-status-message"></span>
          <button class="btn reader-icon-button reader-global-status-dismiss"
                  type="button"
                  aria-label="Dismiss message">×</button>
        </div>
        <main id="reader-main">
          <a class="btn reader-icon-button state-toggle"
             href="#"
             aria-label="Save for later"
             data-id="42"
             data-is-set="false"
             data-action-set="saved"
             data-action-unset="unsave">save</a>
        </main>
      </body>
    </html>
  `);
  await page.addStyleTag({ path: asset("llar.css") });
  await page.addScriptTag({ path: asset("jquery", "jquery.min.js") });
  await page.evaluate(() => {
    window.jQuery.fn.popover = function () {
      return this;
    };
    window.readerFailureRequest = window.jQuery.Deferred();
    window.jQuery.post = function () {
      return window.readerFailureRequest.promise();
    };
  });
  await page.addScriptTag({ path: asset("llar.js") });
  await page.waitForTimeout(50);

  const action = page.locator(".state-toggle");
  const notice = page.locator("#reader-global-status");
  await action.focus();
  await action.click();

  assert.equal(await action.getAttribute("aria-busy"), "true");
  assert.equal(await action.getAttribute("aria-disabled"), "true");
  assert.equal(await notice.isHidden(), true);

  await page.evaluate(() => {
    window.readerFailureRequest.reject({ status: 503 });
  });
  await notice.waitFor({ state: "visible" });
  assert.equal(
    await notice.locator(".reader-global-status-message").textContent(),
    "Could not update this item. Nothing changed.",
  );
  assert.equal(await action.getAttribute("aria-busy"), null);
  assert.equal(await action.getAttribute("aria-disabled"), null);
  assert.equal(
    await action.evaluate((element) => element === document.activeElement),
    true,
  );

  const box = await notice.boundingBox();
  assert.ok(box.x >= 0);
  assert.ok(box.x + box.width <= 320);

  await notice.locator(".reader-global-status-dismiss").click();
  assert.equal(await notice.isHidden(), true);
  assert.equal(
    await action.evaluate((element) => element === document.activeElement),
    true,
  );
  await page.close();
});
