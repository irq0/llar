import { mkdir, writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { chromium } from "playwright";

if (!process.env.LLAR_READER_URL)
  throw new Error(
    "Set LLAR_READER_URL to the Reader address before running this test.",
  );
const base = new URL(process.env.LLAR_READER_URL);
const output = fileURLToPath(
  new URL("../../target/reader-history/", import.meta.url),
);
await mkdir(output, { recursive: true });
const results = [];
const browser = await chromium.launch({
  channel: process.env.LLAR_BROWSER_CHANNEL ?? "chrome",
  headless: true,
  // Exercise the browser page cache, which Playwright disables by default.
  ignoreDefaultArgs: ["--disable-back-forward-cache"],
});

try {
  for (const [device, viewport] of [
    ["desktop", { width: 1440, height: 900 }],
    ["mobile", { width: 390, height: 800 }],
  ]) {
    for (const style of ["preview", "headlines", "gallery"]) {
      const page = await browser.newPage({
        viewport,
        isMobile: device === "mobile",
        hasTouch: device === "mobile",
      });
      page.setDefaultTimeout(15000);
      await page.addInitScript(() => {
        window.historyTestPageShows = [];
        addEventListener("pageshow", (event) =>
          window.historyTestPageShows.push({
            persisted: event.persisted,
            scrollY,
          }),
        );
        document.addEventListener(
          "click",
          (event) => {
            const row = event.target.closest('[id^="item-"][data-id]');
            if (!row) return;
            sessionStorage.setItem(
              "llar-history-test-departure",
              JSON.stringify({
                url: location.href,
                isList: document.body.classList.contains(
                  "reader-mode-list-items",
                ),
                top: row.getBoundingClientRect().top,
                scrollY,
                pageShows: window.historyTestPageShows,
              }),
            );
          },
          true,
        );
      });
      const listUrl = new URL(
        `/reader/group/default/all/source/all/items?list-style=${style}&sort-order=newest`,
        base,
      ).href;
      try {
        await page.goto(listUrl, { waitUntil: "domcontentloaded" });
        await page.waitForTimeout(700);
        const readRows = page.locator(
          '[id^="item-"][data-id][data-unread="false"]',
        );
        const count = await readRows.count();
        if (count < 3)
          throw new Error(
            `Need at least three already-read items; found ${count}.`,
          );
        const row = readRows.nth(Math.min(4, count - 1));
        const id = await row.getAttribute("id");

        async function align() {
          await page
            .locator(`#${id}`)
            .evaluate((el) =>
              scrollBy(
                0,
                el.getBoundingClientRect().top -
                  document.getElementById("top-nav").getBoundingClientRect()
                    .bottom -
                  24,
              ),
            );
          await page.waitForTimeout(300);
          const before = await measure(page, id);
          await page.screenshot({
            path: `${output}/${device}-${style}-before.png`,
          });
          return before;
        }
        async function openItem() {
          await align();
          await page.locator(`#${id} a[href*="/item/by-id/"]`).first().click();
          await page.locator("#item-content-body").waitFor();
          // Measure at activation, after any browser scroll needed to expose the link.
          return page.evaluate(() =>
            JSON.parse(sessionStorage.getItem("llar-history-test-departure")),
          );
        }
        async function record(action, before) {
          await page.waitForTimeout(700);
          const after = await measure(page, id);
          const untested = before.scrollY <= 8;
          const pass = untested
            ? null
            : after.isList &&
              after.url === listUrl &&
              after.top !== null &&
              Math.abs(after.top - before.top) <= 8;
          results.push({
            device,
            style,
            action,
            pass,
            id,
            before,
            after,
            ...(untested
              ? {
                  reason:
                    "No scrollable starting position; restoration was not tested.",
                }
              : {}),
          });
          await page.screenshot({
            path: `${output}/${device}-${style}-${action}.png`,
          });
          console.log(
            `${untested ? "UNTESTED" : pass ? "PASS" : "FAIL"}: ${device} ${style}: ${action}`,
          );
        }

        let before = await openItem();
        await page.goBack({ waitUntil: "commit" });
        await page.waitForFunction(
          (url) =>
            location.href === url &&
            document.body.classList.contains("reader-mode-list-items"),
          listUrl,
        );
        await record("browser-back", before);

        before = await openItem();
        await page.keyboard.press("p");
        await page.waitForFunction(
          (url) =>
            location.href === url &&
            document.body.classList.contains("reader-mode-list-items"),
          listUrl,
        );
        await record("keyboard-back", before);

        before = await align();
        await page.reload({ waitUntil: "domcontentloaded" });
        await record("browser-reload", before);

        before = await openItem();
        if (device === "mobile") {
          await page.locator(".reader-mobile-navigation-toggle").click();
          await page
            .locator("#groupnav")
            .getByRole("link", { name: "all", exact: true })
            .click();
        } else {
          await page.locator(".reader-navbar-path a").first().click();
        }
        await page.waitForLoadState("domcontentloaded");
        await record("list-link", before);
      } catch (error) {
        results.push({ device, style, pass: false, error: error.message });
        console.log(`ERROR: ${device} ${style}: ${error.message}`);
      } finally {
        await page.close();
      }
    }
  }
} finally {
  await browser.close();
  await writeFile(
    `${output}/live-results.json`,
    JSON.stringify(results, null, 2) + "\n",
  );
}
process.exitCode = results.some((result) => result.pass !== true) ? 1 : 0;

async function measure(page, id) {
  return page.evaluate(
    (id) => ({
      url: location.href,
      isList: document.body.classList.contains("reader-mode-list-items"),
      top: document.getElementById(id)?.getBoundingClientRect().top ?? null,
      scrollY,
      pageShows: window.historyTestPageShows,
    }),
    id,
  );
}
