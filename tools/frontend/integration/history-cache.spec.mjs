import { test, expect } from "./fixtures.mjs";
import { openList, openItem, restored } from "./history-helpers.mjs";

test.use({
  launchOptions: { ignoreDefaultArgs: ["--disable-back-forward-cache"] },
});
test.skip(
  ({ browserName }) => browserName !== "chromium",
  "Cache-hit coverage is enforced in Chromium; WebKit eligibility differs.",
);

test("cached Back restores the real list and remains usable", async ({
  page,
  reader,
  isMobile,
}) => {
  await openList(page, reader);
  const before = await openItem(page, isMobile);
  await page.goBack({ waitUntil: "commit" });
  await restored(page, before);
  expect(
    await page.evaluate(() => window.readerTestCachedReturn),
    "This case must restore a cached document",
  ).toBe(true);
  const second = await openItem(page, isMobile, 14);
  await page.goBack({ waitUntil: "commit" });
  await restored(page, second);
  expect(await page.evaluate(() => window.readerTestCachedReturn)).toBe(true);
});
