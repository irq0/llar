import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { chromium, webkit } from "playwright";

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
  const mobile = viewport.width < 768;
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
    const isInspect = url.pathname.endsWith("/dump");
    const articlePath = url.pathname.replace(/\/dump$/, "");
    const listName =
      url.pathname === "/reader/"
        ? "/reader/group/default/all/source/all/items"
        : articlePath.replace(/\/item\/by-id\/\d+$/, "/items");
    const itemPrefix = listName.replace(/\/items$/, "/item/by-id/");
    const listQuery = new URLSearchParams(url.search);
    for (const key of ["range-before", "rank-score", "rank-id", "ranked-at"])
      listQuery.delete(key);
    // LLAR's generated list links omit the originating batch cursor.
    const currentListPath = `${listName}?${listQuery}`;
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
    const destinations = `<a class="${mobile ? "nav-link" : ""}" id="return-list" href="${currentListPath}">all</a>
      <a class="${mobile ? "nav-link" : ""}" id="different-list" href="/reader/group/default/other/source/all/items">Other list</a>
      <a class="${mobile ? "nav-link" : ""}" id="other-filter" href="${listName}?${otherFilter}">Other filter</a>`;
    return route.fulfill({
      contentType: "text/html",
      headers: {
        "Cache-Control": "no-store",
        "Referrer-Policy": "no-referrer",
      },
      body: `<!doctype html><html><head>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="/static/bootstrap/css/bootstrap.min.css">
        <link rel="stylesheet" href="/static/llar.css">
        <style>body { margin:0 } #top-nav { position:fixed; top:0; height:48px; width:100%; background:white; z-index:1100 }</style>
      </head><body class="llar-reader reader-mode-${isInspect ? "dump-item" : isItem ? "show-item" : "list-items"}" data-reader-list-url="${currentListPath}">
        <nav id="top-nav">${mobile ? '<a class="reader-mobile-navigation-toggle" href="#groupnav" data-bs-toggle="collapse" data-bs-target="#groupnav" aria-controls="groupnav" aria-expanded="false" aria-label="Open Reader navigation">Menu</a>' : destinations}
          <a id="skip-content" href="#reader-main">Skip to content</a>
          ${isItem ? `<a id="btn-next-item" href="${itemPrefix}${Number(articlePath.split("/").at(-1)) + 1}${url.search}">Next item</a><a id="inspect-item" href="${articlePath}/dump${url.search}">Inspect item</a>` : ""}
          <a class="btn-reload-current-view" href="${currentListPath}">Refresh list</a></nav>
        ${mobile ? `<nav id="groupnav" class="collapse sidebar sidebar-left"><div id="left-nav">${destinations}</div></nav>` : ""}
        <main id="reader-main">${isInspect ? '<div class="reader-inspect-workspace">Inspect item</div>' : isItem ? '<div id="item-content-body"><p>Article text</p><a href="http://[">Malformed source link</a></div>' : rows}</main>
        <script src="/static/jquery/jquery.min.js"></script>
        <script src="/static/bootstrap/js/bootstrap.bundle.min.js"></script>
        <script src="/static/llar.js"></script>
      </body></html>`,
    });
  });
  page.setDefaultTimeout(10000);
  await page.goto(origin + list);
  await settle(page);
  return page;
}

async function settle(page) {
  await page.waitForFunction(
    () =>
      new Promise((resolve, reject) => {
        let lastY = scrollY;
        let stableFrames = 0;
        const started = performance.now();
        function frame() {
          stableFrames = scrollY === lastY ? stableFrames + 1 : 0;
          lastY = scrollY;
          if (stableFrames >= 4) resolve(true);
          else if (performance.now() - started > 3000)
            reject(new Error("Scrolling did not settle"));
          else requestAnimationFrame(frame);
        }
        requestAnimationFrame(frame);
      }),
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

async function activate(page, selector) {
  const link = page.locator(selector);
  const mobile = page.viewportSize().width < 768;
  if (
    mobile &&
    (await link.evaluate((el) => !!el.closest("#groupnav"))) &&
    !(await link.isVisible())
  ) {
    await page.locator(".reader-mobile-navigation-toggle").tap();
    await page.waitForFunction(() =>
      document.body.classList.contains("reader-mobile-navigation-open"),
    );
  }
  const href = await link.getAttribute("href");
  // replaceState also emits a navigation event; wait for the new document instead.
  await Promise.all([
    href.startsWith("#")
      ? page.waitForURL(new URL(href, page.url()).href)
      : page.waitForEvent("load"),
    mobile ? link.tap() : link.click(),
  ]);
}

async function openFromPosition(page, id = 5) {
  await page.locator(`#item-${id}`).evaluate((el) => {
    window.scrollBy({
      top: el.getBoundingClientRect().top + 20,
      behavior: "instant",
    });
  });
  const before = await position(page, id);
  // Open immediately: do not wait for the 200 ms scroll-save timer.
  await activate(page, `#item-${id} a`);
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

for (const [device, engine, viewport] of [
  ["desktop Chromium", chromium, { width: 1440, height: 900 }],
  ["mobile Chromium", chromium, { width: 390, height: 800 }],
  ["mobile WebKit", webkit, { width: 390, height: 800 }],
]) {
  test(`Reader history UX: ${device}`, async (t) => {
    const browser = await engine.launch({
      ...(engine === chromium
        ? { channel: process.env.LLAR_BROWSER_CHANNEL ?? "chrome" }
        : {}),
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
        const errors = [];
        page.on("pageerror", (error) => errors.push(error.message));
        try {
          const before = await openFromPosition(page);
          await activate(page, "#return-list");
          await settle(page);
          samePosition(before, await position(page));
          assert.deepEqual(errors, []);
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
          await activate(page, ".btn-reload-current-view");
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
          await activate(page, "#different-list");
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
          await activate(page, "#return-list");
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
            else await activate(page, "#return-list");
            await settle(page);
            assert.equal(page.url(), before.url);
            samePosition(before, await position(page, 105));
            assert.deepEqual(await itemIds(page), expectedIds);
          } finally {
            await page.close();
          }
        },
      );
    }

    for (const action of ["Reload article", "Next article", "Inspect item"]) {
      await t.test(
        `${action} keeps the originating batch and position`,
        async () => {
          const page = await fixture(browser, viewport, {
            list: rankedListPath,
          });
          try {
            const before = await openFromPosition(page, 105);
            if (action === "Reload article") await page.reload();
            else if (action === "Inspect item") {
              await activate(page, "#inspect-item");
              await page.locator(".reader-inspect-workspace").waitFor();
            } else {
              await page.keyboard.press("n");
              await page.waitForURL("**/item/by-id/106?*");
            }
            await activate(page, "#return-list");
            await settle(page);
            assert.equal(page.url(), before.url);
            samePosition(before, await position(page, 105));
          } finally {
            await page.close();
          }
        },
      );
    }

    for (const where of ["list", "article", "article then Reload"]) {
      await t.test(
        `Skip to content on ${where} preserves the return position`,
        async () => {
          const page = await fixture(browser, viewport, {
            list: rankedListPath,
          });
          try {
            if (where === "list") await activate(page, "#skip-content");
            const before = await openFromPosition(page, 105);
            if (where !== "list") await activate(page, "#skip-content");
            if (where === "article then Reload") await page.reload();
            await activate(page, "#return-list");
            await settle(page);
            assert.equal(page.url(), before.url);
            samePosition(before, await position(page, 105));
          } finally {
            await page.close();
          }
        },
      );
    }

    await t.test(
      "The Reader home URL resumes through its canonical list link",
      async () => {
        const page = await fixture(browser, viewport, {
          list: "/reader/?list-style=preview",
        });
        try {
          const before = await openFromPosition(page);
          await activate(page, "#return-list");
          await settle(page);
          samePosition(before, await position(page));
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "An article link to another filter starts that filter at the top",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          await openFromPosition(page);
          await activate(page, "#other-filter");
          await settle(page);
          assert.equal(new URL(page.url()).searchParams.get("filter"), "today");
          assert.equal(await page.evaluate(() => scrollY), 0);
        } finally {
          await page.close();
        }
      },
    );

    await t.test(
      "Blocked session storage keeps Back and Reload usable",
      async () => {
        const page = await fixture(browser, viewport);
        try {
          await page.addInitScript(() => {
            Object.defineProperty(window, "sessionStorage", {
              get() {
                throw new DOMException("Storage blocked", "SecurityError");
              },
            });
          });
          await page.reload();
          const errors = [];
          page.on("pageerror", (error) => errors.push(error.message));
          const before = await openFromPosition(page);
          await page.goBack();
          await settle(page);
          samePosition(before, await position(page));
          await page.reload();
          await settle(page);
          samePosition(before, await position(page));
          assert.deepEqual(errors, []);
        } finally {
          await page.close();
        }
      },
    );

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
          await activate(page, "#other-filter");
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
