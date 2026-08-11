import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdir, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { chromium } from "playwright";

const viewport = { width: 1440, height: 900 };
const deviceScaleFactor = 2;
const baseUrl = new URL(process.env.LLAR_READER_URL ?? "http://127.0.0.1:8023");
const browserChannel = process.env.LLAR_BROWSER_CHANNEL ?? "chrome";
const outputDir = path.resolve(
  process.env.LLAR_SCREENSHOT_OUT ?? "target/demo-media",
);
const diagnosticsDir = path.join(outputDir, "diagnostics");
const tracePath = path.join(diagnosticsDir, "reader-trace.zip");
const failurePath = path.join(diagnosticsDir, "reader-failure.png");
const captureStyles = fileURLToPath(new URL("capture.css", import.meta.url));
const readerPath =
  "/reader/group/source-tag/demo/source/all/items?list-style=preview";

function digest(data) {
  return createHash("sha256").update(data).digest("hex");
}

function pngDimensions(data) {
  const signature = "89504e470d0a1a0a";

  assert.ok(data.length >= 24, "Screenshot is too small to be a PNG");
  assert.equal(
    data.subarray(0, 8).toString("hex"),
    signature,
    "Screenshot is not a PNG",
  );

  return {
    width: data.readUInt32BE(16),
    height: data.readUInt32BE(20),
  };
}

async function captureStable(page, name) {
  const options = {
    animations: "disabled",
    caret: "hide",
    fullPage: false,
    type: "png",
  };
  const firstCapture = await page.screenshot(options);
  const secondCapture = await page.screenshot(options);

  assert.equal(
    digest(firstCapture),
    digest(secondCapture),
    `Two consecutive ${name} captures differed; refusing to publish an unstable screenshot`,
  );
  assert.deepEqual(
    pngDimensions(secondCapture),
    {
      width: viewport.width * deviceScaleFactor,
      height: viewport.height * deviceScaleFactor,
    },
    `${name} screenshot dimensions are incorrect`,
  );
  assert.ok(
    secondCapture.length >= 50_000,
    `${name} screenshot is unexpectedly small: ${secondCapture.length} bytes`,
  );

  const screenshotPath = path.join(outputDir, `${name}.png`);
  await writeFile(screenshotPath, secondCapture);
  console.log(
    `Captured ${screenshotPath} (${secondCapture.length} bytes, sha256:${digest(secondCapture)})`,
  );
}

async function waitForVisibleImages(page) {
  return page.evaluate(async () => {
    const visibleImages = [...document.images].filter((image) => {
      const bounds = image.getBoundingClientRect();
      return (
        bounds.width > 0 &&
        bounds.height > 0 &&
        bounds.bottom > 0 &&
        bounds.right > 0 &&
        bounds.top < window.innerHeight &&
        bounds.left < window.innerWidth
      );
    });

    await Promise.all(
      visibleImages.map(async (image) => {
        if (!image.complete) {
          await new Promise((resolve, reject) => {
            image.addEventListener("load", resolve, { once: true });
            image.addEventListener("error", reject, { once: true });
          });
        }

        if (typeof image.decode === "function") {
          await image.decode();
        }
      }),
    );

    return visibleImages.map((image) => ({
      src: image.currentSrc || image.src,
      width: image.naturalWidth,
      height: image.naturalHeight,
    }));
  });
}

await mkdir(diagnosticsDir, { recursive: true });

const browser = await chromium.launch({
  channel: browserChannel,
  headless: true,
});
console.log(`Capturing with ${browserChannel} ${browser.version()}`);
const context = await browser.newContext({
  viewport,
  deviceScaleFactor,
  colorScheme: "light",
  locale: "en-US",
  timezoneId: "Europe/Berlin",
  reducedMotion: "reduce",
});
const page = await context.newPage();
const consoleErrors = [];
const pageErrors = [];
const failedRequests = [];
const badResponses = [];

page.on("console", (message) => {
  if (message.type() === "error") {
    consoleErrors.push(message.text());
  }
});
page.on("pageerror", (error) => pageErrors.push(error.message));
page.on("requestfailed", (request) => {
  failedRequests.push(
    `${request.method()} ${request.url()}: ${request.failure()?.errorText}`,
  );
});
page.on("response", (response) => {
  if (response.status() >= 400) {
    badResponses.push(`${response.status()} ${response.url()}`);
  }
});

await context.tracing.start({
  screenshots: true,
  snapshots: true,
  sources: true,
});

try {
  const readerUrl = new URL(readerPath, baseUrl);
  await page.goto(readerUrl.href, { waitUntil: "networkidle" });

  await page
    .getByRole("link", {
      name: "Local-first tools are turning sync into a product decision",
    })
    .waitFor({ state: "visible" });
  await page.locator(".feed-item").first().waitFor({ state: "visible" });

  const itemCount = await page.locator(".feed-item").count();
  assert.ok(
    itemCount >= 8,
    `Expected at least 8 demo items, found ${itemCount}`,
  );

  await page.evaluate(async () => document.fonts.ready);
  assert.equal(
    await page.evaluate(() => document.fonts.status),
    "loaded",
    "Webfonts did not load",
  );

  const visibleImages = await waitForVisibleImages(page);
  const demoArtwork = visibleImages.find(({ src }) =>
    src.includes("/static/demo/"),
  );
  assert.ok(demoArtwork, "No demo artwork is visible in the Reader viewport");
  assert.ok(
    demoArtwork.width > 0 && demoArtwork.height > 0,
    "Visible demo artwork did not decode",
  );

  await page.addStyleTag({ path: captureStyles });
  await page.evaluate(() => window.scrollTo(0, 0));

  assert.deepEqual(
    consoleErrors,
    [],
    `Browser console errors:\n${consoleErrors.join("\n")}`,
  );
  assert.deepEqual(
    pageErrors,
    [],
    `Uncaught page errors:\n${pageErrors.join("\n")}`,
  );
  assert.deepEqual(
    failedRequests,
    [],
    `Failed browser requests:\n${failedRequests.join("\n")}`,
  );
  assert.deepEqual(
    badResponses,
    [],
    `HTTP error responses:\n${badResponses.join("\n")}`,
  );

  await captureStable(page, "reader");

  for (const { name, path: route, ready } of [
    {
      name: "reading-queue",
      path: "/reader/tools/saved-overview",
      ready: ".feed-item",
    },
    {
      name: "continue-reading",
      path: "/reader/tools/continue-reading",
      ready: ".feed-item",
    },
    {
      name: "gems",
      path: "/reader/tools/gems?browse=true",
      ready: ".gems-view",
    },
    {
      name: "todays-vibe",
      path: "/reader/tools/todays-vibe",
      ready: "[data-vibe-cluster-id]",
    },
  ]) {
    await page.goto(new URL(route, baseUrl).href, { waitUntil: "networkidle" });
    await page.locator(ready).first().waitFor({ state: "visible" });
    await page.addStyleTag({ path: captureStyles });
    await page.evaluate(() => window.scrollTo(0, 0));
    await captureStable(page, name);
  }
} catch (error) {
  await page
    .screenshot({ path: failurePath, fullPage: true })
    .catch(() => undefined);
  throw error;
} finally {
  await context.tracing.stop({ path: tracePath }).catch(() => undefined);
  await browser.close();
}
