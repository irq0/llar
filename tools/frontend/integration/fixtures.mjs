import { test as base, expect } from "@playwright/test";
import { startReader } from "./server.mjs";

export const test = base.extend({
  reader: [
    async ({ browserName }, use, workerInfo) => {
      const reader = await startReader(
        `server-${browserName}-${workerInfo.workerIndex}.log`,
      );
      try {
        await use(reader);
      } finally {
        await reader.stop();
      }
    },
    { scope: "worker", timeout: 180000 },
  ],
  seed: [
    async ({ reader }, use) => {
      await use(await reader.reset());
    },
    { auto: true },
  ],
  page: async ({ page, reader }, use, testInfo) => {
    const errors = [];
    const requests = [];
    await page.addInitScript(() => {
      addEventListener("pageshow", (event) => {
        window.readerTestCachedReturn = event.persisted;
      });
      document.addEventListener(
        "click",
        (event) => {
          const row = event.target.closest('[id^="item-"][data-id]');
          if (!row) return;
          console.debug(
            "LLAR_BROWSER_DEPARTURE " +
              JSON.stringify({
                id: row.id,
                url: location.href,
                top: row.getBoundingClientRect().top,
                scrollY,
              }),
          );
        },
        true,
      );
    });
    page.on("pageerror", (error) => errors.push(error.message));
    page.on("response", (response) => {
      if (response.url().startsWith(reader.url) && response.status() >= 400)
        requests.push(`${response.status()} ${response.url()}`);
    });
    await use(page);
    await testInfo.attach("page-errors", {
      body: JSON.stringify({ errors, requests }),
      contentType: "application/json",
    });
    expect(errors, "Uncaught browser errors").toEqual([]);
    expect(requests, "Failed local HTTP responses").toEqual([]);
  },
});
export { expect };
