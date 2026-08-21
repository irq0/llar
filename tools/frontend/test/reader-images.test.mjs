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
const browserExecutable = process.env.LLAR_BROWSER_EXECUTABLE;

test("Reader delegates article image enlargement to the browser", async (t) => {
  const browser = await chromium.launch(
    browserExecutable
      ? { executablePath: browserExecutable, headless: true }
      : { channel: browserChannel, headless: true },
  );
  t.after(() => browser.close());

  const page = await browser.newPage({ viewport: { width: 600, height: 800 } });
  await page.setContent(`
    <!doctype html>
    <html>
      <body class="llar-reader reader-mode-show-item">
        <main id="reader-main">
          <div id="item-content-body" class="item-content-body">
            <img id="standalone" src="https://example.test/large.png" alt="A diagram">
            <picture id="responsive-picture">
              <img id="responsive" src="https://example.test/responsive.png" alt="Responsive diagram">
            </picture>
            <a id="authored-link" href="https://example.test/article">
              <img id="linked" src="https://example.test/linked.png" alt="Linked image">
            </a>
            <img id="map" src="https://example.test/map.png" usemap="#regions" alt="Map">
            <img id="missing-source" alt="Missing source">
            <div id="svg-container" style="width: 200px">
              <svg id="inline-svg" width="400" height="200" viewBox="0 0 400 200"></svg>
            </div>
          </div>
        </main>
      </body>
    </html>
  `);
  await page.addStyleTag({ path: asset("llar.css") });
  await page.addScriptTag({ path: asset("jquery", "jquery.min.js") });
  await page.addScriptTag({ path: asset("llar.js") });
  await page.waitForFunction(
    () => document.querySelectorAll("a.reader-image-enlarge").length === 2,
  );

  const standaloneLink = page.locator("#standalone").locator("xpath=parent::a");
  assert.equal(
    await standaloneLink.getAttribute("class"),
    "reader-image-enlarge",
  );
  assert.equal(await standaloneLink.getAttribute("target"), "_blank");
  assert.equal(await standaloneLink.getAttribute("rel"), "noopener noreferrer");
  assert.equal(
    await standaloneLink.getAttribute("aria-label"),
    "Enlarge image: A diagram",
  );
  assert.equal(
    await standaloneLink.getAttribute("href"),
    "https://example.test/large.png",
  );

  assert.equal(
    await page
      .locator("#responsive-picture")
      .locator("xpath=parent::a")
      .getAttribute("class"),
    "reader-image-enlarge",
  );
  assert.equal(
    await page.locator("#linked").locator("xpath=parent::a").getAttribute("id"),
    "authored-link",
  );
  assert.equal(
    await page.locator("#map").locator("xpath=parent::a").count(),
    0,
  );
  assert.equal(
    await page.locator("#missing-source").locator("xpath=parent::a").count(),
    0,
  );

  const svgBox = await page.locator("#inline-svg").boundingBox();
  assert.equal(svgBox.width, 200);
  assert.equal(svgBox.height, 100);

  await standaloneLink.focus();
  assert.equal(
    await standaloneLink.evaluate(
      (element) => element === document.activeElement,
    ),
    true,
  );
});
