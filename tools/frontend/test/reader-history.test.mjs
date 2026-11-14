import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";

const assets = fileURLToPath(
  new URL("../../../resources/status/", import.meta.url),
);
const origin = "http://llar.test";
const listPath =
  "/reader/group/default/all/source/all/items?filter=unread&list-style=preview";
const rankedListPath = `${listPath}&sort-order=ranked&rank-score=-12.75&rank-id=4242&ranked-at=2026-11-14T12%3A00%3A00Z`;

async function fixture(
  browser,
  viewport,
  { removedId = null, delayedImage = false, list = listPath } = {},
) {
  const page = await browser.newPage({
    viewport,
    isMobile: viewport.width < 768,
    hasTouch: viewport.width < 768,
  });
  let opened = false;
  await page.route(`${origin}/late-image.svg`, async (route) => {
    if (opened) await new Promise((resolve) => setTimeout(resolve, 750));
    await route.fulfill({
      contentType: "image/svg+xml",
      body: '<svg xmlns="http://www.w3.org/2000/svg" width="400" height="400"><rect width="400" height="400" fill="silver"/></svg>',
    });
  });
  await page.route(`${origin}/static/**`, (route) =>
    route.fulfill({
      path: path.join(assets, new URL(route.request().url()).pathname.slice(8)),
    }),
  );
  await page.route(`${origin}/reader/**`, (route) => {
    const url = new URL(route.request().url());
    const isItem = url.pathname.includes("/item/by-id/");
    const listName = url.pathname.replace(/\/item\/by-id\/\d+$/, "/items");
    const itemPrefix = listName.replace(/\/items$/, "/item/by-id/");
    const currentListPath = listName + url.search;
    const otherFilter = new URLSearchParams(url.search);
    otherFilter.set(
      "filter",
      url.searchParams.get("filter") === "today" ? "unread" : "today",
    );
    const firstId = url.searchParams.has("rank-id")
      ? 101
      : url.searchParams.get("filter") === "today"
        ? 201
        : 1;
    if (isItem) opened = true;
    const rows = Array.from({ length: 16 }, (_, i) => i + firstId)
      .filter((id) => !(opened && id === removedId))
      .map(
        (id) =>
          `${delayedImage && id === 5 ? `<img id="late-image" src="/late-image.svg" ${opened ? 'loading="lazy"' : ""} style="display:block;width:400px;max-width:100%" alt="Delayed preview">` : ""}<div class="feed-item" id="item-${id}" data-id="${id}" style="height:1000px; margin:0; padding:100px 16px; box-sizing:border-box"><a href="${itemPrefix}${id}${url.search}">Open item ${id}</a></div>`,
      )
      .join("");
    return route.fulfill({
      contentType: "text/html",
      headers: { "Cache-Control": "no-store" },
      body: `<!doctype html><html><head>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="/static/llar.css">
        <style>body { margin:0 } #top-nav { position:fixed; top:0; height:48px; width:100%; background:white; z-index:1100 }</style>
      </head><body class="llar-reader reader-mode-${isItem ? "show-item" : "list-items"}">
        <nav id="top-nav"><a id="return-list" href="${currentListPath}">all</a>
          <a id="different-list" href="/reader/group/default/other/source/all/items">Other list</a>
          <a id="other-filter" href="${listName}?${otherFilter}">Other filter</a>
          <a class="btn-reload-current-view" href="${currentListPath}">Refresh list</a></nav>
        <main id="reader-main">${isItem ? '<div id="item-content-body"><p>Article text</p></div>' : rows}</main>
        <script src="/static/jquery/jquery.min.js"></script>
        <script src="/static/llar.js"></script>
      </body></html>`,
    });
  });
  await page.goto(origin + list);
  await settle(page);
  return page;
}

async function settle(page) {
  await page.evaluate(
    () =>
      new Promise((resolve) =>
        requestAnimationFrame(() =>
          requestAnimationFrame(() => requestAnimationFrame(resolve)),
        ),
      ),
  );
}

async function position(page, id = 5) {
  return page.locator(`#item-${id}`).evaluate((el) => ({
    top: el.getBoundingClientRect().top,
    scrollY,
    url: location.href,
  }));
}

async function itemIds(page) {
  return page
    .locator(".feed-item")
    .evaluateAll((rows) => rows.map((row) => Number(row.dataset.id)));
}

async function openFromPosition(page, id = 5) {
  await page.locator(`#item-${id}`).evaluate((el) => {
    window.scrollBy(0, el.getBoundingClientRect().top + 20);
  });
  const before = await position(page, id);
  // Open immediately: do not wait for the 200 ms scroll-save timer.
  await page.locator(`#item-${id} a`).click();
  await page.locator("#item-content-body").waitFor();
  return before;
}

function samePosition(before, after) {
  assert.equal(
    after.url,
    before.url,
    "Return to the same filter, sort, and batch",
  );
  assert.ok(
    Math.abs(after.top - before.top) <= 8,
    `The same item must stay within 8 px of its old position: ${JSON.stringify({ before, after })}`,
  );
}

for (const [device, viewport] of [
  ["desktop", { width: 1440, height: 900 }],
  ["mobile", { width: 390, height: 800 }],
]) {
  test(`Reader history UX: ${device}`, async (t) => {
    const browser = await chromium.launch({
      channel: process.env.LLAR_BROWSER_CHANNEL ?? "chrome",
      headless: true,
    });
    t.after(() => browser.close());

    await t.test(
      "Back and Forward retain the original list position",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          const before = await openFromPosition(page);
          await page.goBack();
          await settle(page);
          samePosition(before, await position(page));
          await page.goForward();
          await page.locator("#item-content-body").waitFor();
          await page.goBack();
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "The p shortcut returns to the same list position",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          const before = await openFromPosition(page);
          await page.keyboard.press("p");
          await page.waitForURL(origin + listPath);
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "LLAR restores a surviving item without native restoration",
      async () => {
        const page = await fixture(browser, viewport, { removedId: 4 });
        try {
          await page.evaluate(() => {
            history.scrollRestoration = "manual";
          });
          const before = await openFromPosition(page);
          await page.goBack();
          await settle(page);
          assert.equal(
            await page.evaluate(() => history.scrollRestoration),
            "manual",
          );
          assert.equal(await page.locator("#item-4").count(), 0);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "Clicking the originating list name resumes its position",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          const before = await openFromPosition(page);
          await page.locator("#return-list").click();
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "If the top item disappears, Back shows the next surviving item",
      async () => {
        const page = await fixture(browser, viewport, { removedId: 5 });
        try {
          await openFromPosition(page);
          await page.goBack();
          await settle(page);
          const after = await position(page, 6);
          assert.ok(
            Math.abs(after.top - 48) <= 8,
            `The next item must start below the 48 px toolbar, not below the screen: ${JSON.stringify(after)}`,
          );
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "Browser Reload preserves the current reading position",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          await openFromPosition(page);
          await page.goBack();
          await settle(page);
          const before = await position(page);
          await page.reload();
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "Explicit list refresh starts at the top without adding history",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          await openFromPosition(page);
          await page.goBack();
          await settle(page);
          const length = await page.evaluate(() => history.length);
          await page.locator(".btn-reload-current-view").click();
          await settle(page);
          assert.equal(await page.evaluate(() => scrollY), 0);
          assert.equal(await page.evaluate(() => history.length), length);
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "A different list starts at the top; Back restores the previous list",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          await openFromPosition(page);
          await page.goBack();
          await settle(page);
          const before = await position(page);
          await page.locator("#different-list").click();
          await settle(page);
          assert.equal(await page.evaluate(() => scrollY), 0);
          await page.goBack();
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "Separate visits to the same list keep separate positions",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          const first = await openFromPosition(page);
          await page.locator("#return-list").click();
          await settle(page);
          const second = await openFromPosition(page, 9);
          await page.goBack();
          await settle(page);
          samePosition(second, await position(page, 9));
          await page.goBack();
          await page.locator("#item-content-body").waitFor();
          await page.goBack();
          await settle(page);
          samePosition(first, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    for (const action of ["Back", "list name"]) {
      await t.test(
        `${action} returns to the same later ranked batch`,
        async () => {
          const page = await fixture(browser, viewport, {
            list: rankedListPath,
          });
          const expectedIds = Array.from({ length: 16 }, (_, i) => i + 101);
          try {
            assert.deepEqual(await itemIds(page), expectedIds);
            const before = await openFromPosition(page, 105);
            assert.equal(
              new URL(page.url()).search,
              new URL(origin + rankedListPath).search,
            );
            if (action === "Back") await page.goBack();
            else await page.locator("#return-list").click();
            await settle(page);
            samePosition(before, await position(page, 105));
            assert.deepEqual(await itemIds(page), expectedIds);
          } finally {
            await page.close();
          }
        },
      );
    }

    await t.test(
      "Filters on the same path retain separate items and positions",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          const unreadIds = Array.from({ length: 16 }, (_, i) => i + 1);
          const todayIds = Array.from({ length: 16 }, (_, i) => i + 201);
          assert.deepEqual(await itemIds(page), unreadIds);
          const unread = await openFromPosition(page);
          await page.goBack();
          await settle(page);
          await page.locator("#other-filter").click();
          await settle(page);
          assert.equal(
            new URL(page.url()).pathname,
            new URL(unread.url).pathname,
          );
          assert.equal(new URL(page.url()).searchParams.get("filter"), "today");
          assert.equal(await page.evaluate(() => scrollY), 0);
          assert.deepEqual(await itemIds(page), todayIds);
          const today = await openFromPosition(page, 209);
          await page.goBack();
          await settle(page);
          samePosition(today, await position(page, 209));
          await page.goBack();
          await settle(page);
          samePosition(unread, await position(page));
          assert.deepEqual(await itemIds(page), unreadIds);
          await page.goForward();
          await settle(page);
          samePosition(today, await position(page, 209));
          assert.deepEqual(await itemIds(page), todayIds);
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "A preview image loading after Back does not move the reading position",
      async () => {
        const page = await fixture(browser, viewport, { delayedImage: true });
        try {
          const before = await openFromPosition(page);
          await page.goBack();
          await settle(page);
          samePosition(before, await position(page));
          await page.waitForFunction(() => {
            const image = document.getElementById("late-image");
            return image.complete && image.naturalHeight > 0;
          });
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );
  });
}
