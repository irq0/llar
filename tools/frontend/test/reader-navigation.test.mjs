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

async function addScript(page, ...parts) {
  await page.addScriptTag({ path: asset(...parts) });
}

test("Mobile drawers move and restore keyboard focus", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage({ viewport: { width: 390, height: 800 } });
  await page.setContent(`
    <!doctype html>
    <html>
      <head><meta name="llar-mode" content="list-items"></head>
      <body class="llar-reader">
        <nav id="top-nav">
          <a class="reader-mobile-navigation-toggle"
             href="#groupnav"
             data-bs-toggle="collapse"
             data-bs-target="#groupnav"
             aria-controls="groupnav"
             aria-expanded="false"
             aria-label="Open Reader navigation">Menu</a>
          <a class="reader-mobile-sources-toggle"
             href="#sourcenav"
             data-bs-toggle="collapse"
             data-bs-target="#sourcenav"
             aria-controls="sourcenav"
             aria-expanded="false"
             aria-label="Open source filters">Sources</a>
        </nav>
        <nav class="collapse" id="groupnav">
          <a class="nav-link" href="/reader/group/default/all/source/all/items">All items</a>
        </nav>
        <main id="reader-main" tabindex="-1"></main>
        <nav class="collapse" id="sourcenav">
          <a class="nav-link" href="/reader/group/default/all/source/example/items">Example</a>
        </nav>
      </body>
    </html>
  `);
  await page.addStyleTag({
    path: asset("bootstrap", "css", "bootstrap.min.css"),
  });
  await addScript(page, "jquery", "jquery.min.js");
  await addScript(page, "bootstrap", "js", "bootstrap.bundle.min.js");
  await addScript(page, "llar.js");

  for (const [toggleSelector, drawerSelector] of [
    [".reader-mobile-navigation-toggle", "#groupnav"],
    [".reader-mobile-sources-toggle", "#sourcenav"],
  ]) {
    const toggle = page.locator(toggleSelector);
    const firstDestination = page
      .locator(`${drawerSelector} .nav-link`)
      .first();

    await toggle.focus();
    await page.keyboard.press("Enter");
    await page.locator(drawerSelector).waitFor({ state: "visible" });
    await assertFocused(firstDestination);
    assert.match(await toggle.getAttribute("aria-label"), /^Close /);

    await page.keyboard.press("Escape");
    await page.locator(drawerSelector).waitFor({ state: "hidden" });
    await assertFocused(toggle);
    assert.match(await toggle.getAttribute("aria-label"), /^Open /);
  }

  await page.close();
});

test("Reader list position falls back to a surviving item anchor", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage({ viewport: { width: 900, height: 500 } });
  await page.route("http://llar.test/reader/items", (route) =>
    route.fulfill({
      contentType: "text/html",
      body: `
        <!doctype html>
        <html>
          <head><meta name="llar-mode" content="list-items"></head>
          <body class="llar-reader reader-mode-list-items">
            <main id="reader-main">
              <div id="item-1" data-id="1" style="height:600px">One</div>
              <div id="item-2" data-id="2" style="height:600px">Two</div>
              <div id="item-3" data-id="3" style="height:600px">Three</div>
            </main>
          </body>
        </html>
      `,
    }),
  );
  await page.goto("http://llar.test/reader/items");
  await addScript(page, "jquery", "jquery.min.js");
  await addScript(page, "bootstrap", "js", "bootstrap.bundle.min.js");
  await addScript(page, "llar.js");

  await page.evaluate(() => window.scrollTo(0, 650));
  const expectedTop = await page
    .locator("#item-2")
    .evaluate((item) => item.getBoundingClientRect().top);
  await page.evaluate(() => saveReaderListPosition());

  // Simulate an unread list changing while its opened item is displayed.
  await page.locator("#item-2").evaluate((item) => item.remove());
  await page.evaluate(() => {
    window.scrollTo(0, 0);
    restoreReaderListPosition();
  });

  assert.equal(
    Math.round(
      await page
        .locator("#item-3")
        .evaluate((item) => item.getBoundingClientRect().top),
    ),
    Math.round(expectedTop + 600),
  );
  await page.close();
});

async function assertFocused(locator) {
  assert.equal(
    await locator.evaluate(
      (element) =>
        new Promise((resolve) => {
          const deadline = Date.now() + 1500;
          function checkFocus() {
            if (element === document.activeElement) resolve(true);
            else if (Date.now() >= deadline) resolve(false);
            else window.requestAnimationFrame(checkFocus);
          }
          checkFocus();
        }),
    ),
    true,
  );
}
