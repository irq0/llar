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

test("Mobile article double-tap advances without hijacking controls", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const context = await browser.newContext({
    viewport: { width: 390, height: 800 },
    hasTouch: true,
    isMobile: true,
  });
  const page = await context.newPage();
  await page.setContent(`
    <!doctype html>
    <html>
      <body class="llar-reader reader-mode-show-item">
        <main id="reader-main">
          <div id="item-content-body">
            <p id="article-copy">Article copy</p>
            <a id="article-link" href="#linked">Linked copy</a>
          </div>
        </main>
      </body>
    </html>
  `);
  await addScript(page, "jquery", "jquery.min.js");
  await addScript(page, "llar.js");
  await page.waitForFunction(() => readingNavigation.container !== null);
  await page.evaluate(() => {
    window.mobileAdvanceCount = 0;
    window.advanceReadingBlock = () => {
      window.mobileAdvanceCount += 1;
      return true;
    };
  });

  await page.locator("#article-copy").evaluate((element) => {
    element.dispatchEvent(
      new PointerEvent("dblclick", {
        bubbles: true,
        cancelable: true,
        pointerType: "touch",
      }),
    );
  });
  assert.equal(await page.evaluate(() => window.mobileAdvanceCount), 1);

  await page.locator("#article-copy").evaluate((element) => {
    element.dispatchEvent(
      new PointerEvent("dblclick", {
        bubbles: true,
        cancelable: true,
        pointerType: "mouse",
      }),
    );
  });
  await page.locator("#article-link").evaluate((element) => {
    element.dispatchEvent(
      new PointerEvent("dblclick", {
        bubbles: true,
        cancelable: true,
        pointerType: "touch",
      }),
    );
  });
  await page.locator("#article-copy").dispatchEvent("pointerdown", {
    pointerType: "touch",
    clientX: 200,
    clientY: 100,
  });
  await page.locator("#article-copy").dispatchEvent("pointerup", {
    pointerType: "touch",
    clientX: 100,
    clientY: 100,
  });
  assert.equal(await page.evaluate(() => window.mobileAdvanceCount), 1);

  await context.close();
});

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
              <div id="item-4" data-id="4" style="height:600px">Four</div>
              <div id="item-5" data-id="5" style="height:600px">Five</div>
              <div id="item-6" data-id="6" style="height:600px">Six</div>
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
  assert.equal(
    await page.evaluate(
      () => history.state.llarReaderListPosition.anchors.length,
    ),
    5,
  );

  // Simulate an unread list changing while its opened item is displayed.
  await page.locator("#item-2").evaluate((item) => item.remove());
  await page.evaluate(() => {
    // Native restoration can put the document back at the old numeric offset,
    // which is wrong once an item above the anchor has disappeared.
    window.scrollTo(0, 650);
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

test("Reader list restores only for history traversal", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage({ viewport: { width: 900, height: 500 } });
  let itemTwoVisible = true;

  await page.route("http://llar.test/static/jquery.js", (route) =>
    route.fulfill({
      contentType: "text/javascript",
      path: asset("jquery", "jquery.min.js"),
    }),
  );
  await page.route("http://llar.test/static/bootstrap.js", (route) =>
    route.fulfill({
      contentType: "text/javascript",
      path: asset("bootstrap", "js", "bootstrap.bundle.min.js"),
    }),
  );
  await page.route("http://llar.test/static/llar.js", (route) =>
    route.fulfill({
      contentType: "text/javascript",
      path: asset("llar.js"),
    }),
  );
  await page.route("http://llar.test/reader/item/2", (route) => {
    itemTwoVisible = false;
    return route.fulfill({
      contentType: "text/html",
      headers: { "Cache-Control": "no-store" },
      body: "<!doctype html><html><body>Item two</body></html>",
    });
  });
  await page.route("http://llar.test/reader/items", (route) =>
    route.fulfill({
      contentType: "text/html",
      headers: { "Cache-Control": "no-store" },
      body: `
        <!doctype html>
        <html>
          <head><meta name="llar-mode" content="list-items"></head>
          <body class="llar-reader reader-mode-list-items">
            <a id="reload-view" class="btn-reload-current-view" href="/reader/items">Reload</a>
            <main id="reader-main">
              <div id="item-1" data-id="1" style="height:600px">One</div>
              ${
                itemTwoVisible
                  ? '<div id="item-2" data-id="2" style="height:600px"><div style="height:100px"></div><a href="/reader/item/2">Two</a></div>'
                  : ""
              }
              <div id="item-3" data-id="3" style="height:600px">Three</div>
            </main>
            <script src="/static/jquery.js"></script>
            <script src="/static/bootstrap.js"></script>
            <script src="/static/llar.js"></script>
          </body>
        </html>
      `,
    }),
  );

  await page.goto("http://llar.test/reader/items");
  await page.evaluate(
    () =>
      new Promise((resolve) =>
        requestAnimationFrame(() => requestAnimationFrame(resolve)),
      ),
  );
  await page.evaluate(() => window.scrollTo(0, 650));
  await page.waitForTimeout(250);
  assert.equal(
    Math.round(
      await page.evaluate(() => history.state.llarReaderListPosition.scrollY),
    ),
    650,
  );

  // Link activation must flush a newer position without waiting for the
  // throttled scroll save or relying exclusively on pagehide.
  await page.evaluate(() => window.scrollTo(0, 700));
  const expectedTop = await page
    .locator("#item-2")
    .evaluate((item) => item.getBoundingClientRect().top);
  await page.locator("#item-2 a").click();

  await page.goBack({ waitUntil: "load" });
  await page.evaluate(
    () =>
      new Promise((resolve) =>
        requestAnimationFrame(() =>
          requestAnimationFrame(() => requestAnimationFrame(resolve)),
        ),
      ),
  );
  const restored = await page.evaluate(() => ({
    itemTop: document.getElementById("item-3").getBoundingClientRect().top,
    navigationType: performance.getEntriesByType("navigation")[0]?.type,
    savedPosition: history.state?.llarReaderListPosition,
    scrollY: window.scrollY,
  }));
  assert.equal(
    Math.round(restored.itemTop),
    Math.round(expectedTop + 600),
    JSON.stringify(restored),
  );
  assert.equal(Math.round(restored.savedPosition.scrollY), 700);

  await page.evaluate(() => window.scrollTo(0, 650));
  await page.reload({ waitUntil: "load" });
  await page.waitForFunction(() => window.scrollY === 0);

  await page.evaluate(() => window.scrollTo(0, 650));
  const historyLengthBeforeReload = await page.evaluate(() => history.length);
  await Promise.all([
    page.waitForNavigation({ waitUntil: "load" }),
    page.evaluate(() => document.getElementById("reload-view").click()),
  ]);
  await page.waitForTimeout(100);
  const reloaded = await page.evaluate(() => ({
    historyLength: history.length,
    navigationType: performance.getEntriesByType("navigation")[0]?.type,
    scrollY: window.scrollY,
  }));
  assert.equal(reloaded.scrollY, 0, JSON.stringify(reloaded));
  assert.equal(reloaded.navigationType, "reload");
  assert.equal(reloaded.historyLength, historyLengthBeforeReload);

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
