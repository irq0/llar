import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

import { chromium } from "playwright";

const repositoryRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);
const readerStyles = path.join(repositoryRoot, "resources/status/llar.css");
const browserChannel = process.env.LLAR_BROWSER_CHANNEL ?? "chrome";
const longToken = "source-with-an-unusually-long-unbroken-identifier".repeat(4);

async function renderResponsiveFixture(page, width) {
  await page.setViewportSize({ width, height: 800 });
  await page.setContent(`
    <!doctype html>
    <html>
      <head>
        <style>
          *, *::before, *::after { box-sizing: border-box; }
          body { margin: 0; }
        </style>
      </head>
      <body class="llar-reader">
        <main class="reader-tool-main">
          <section class="reader-tool-workbench">
            <div class="reader-tool-section-heading">
              <h2>${longToken}</h2>
              <span>100 items in 23 clusters</span>
            </div>
            <div class="reader-search-sources">
              <a class="reader-search-source" href="#">
                ${longToken}<span class="reader-search-source-count">42</span>
              </a>
            </div>
            <ol class="reader-queue-cluster-links">
              <li class="reader-queue-cluster-link-item">
                <a class="reader-queue-cluster-link" href="#">
                  <span class="reader-queue-cluster-link-label">A cluster</span>
                  <span class="reader-queue-cluster-link-meta">12 items</span>
                </a>
              </li>
            </ol>
            <article class="reader-vibe-story">
              <div class="reader-vibe-story-heading">
                <h2 class="reader-vibe-story-title">
                  A deliberately long story title
                  <span class="reader-vibe-representative-source">${longToken}</span>
                </h2>
                <time class="reader-vibe-story-age">2h ago</time>
              </div>
            </article>
            <div class="item-content-body">
              <p>${longToken}</p>
            </div>
          </section>
        </main>
      </body>
    </html>
  `);
  await page.addStyleTag({ path: readerStyles });
}

test("Reader workbench contains difficult content at narrow widths", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  for (const width of [320, 390, 768]) {
    await t.test(`${width}px viewport has no page-level overflow`, async () => {
      const page = await browser.newPage();
      await renderResponsiveFixture(page, width);

      const dimensions = await page.evaluate(() => ({
        viewport: window.innerWidth,
        page: document.documentElement.scrollWidth,
        workbench: document.querySelector(".reader-tool-workbench").scrollWidth,
        workbenchVisible: document.querySelector(".reader-tool-workbench")
          .clientWidth,
      }));

      assert.ok(dimensions.page <= dimensions.viewport);
      assert.ok(dimensions.workbench <= dimensions.workbenchVisible);
      await page.close();
    });
  }
});

test("Vibe separates source and age from the title on phones", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage();
  await renderResponsiveFixture(page, 390);

  const title = await page.locator(".reader-vibe-story-title").boundingBox();
  const source = await page
    .locator(".reader-vibe-representative-source")
    .boundingBox();
  const age = await page.locator(".reader-vibe-story-age").boundingBox();

  assert.ok(source.y > title.y);
  assert.ok(age.y >= title.y + title.height);
  await page.close();
});
