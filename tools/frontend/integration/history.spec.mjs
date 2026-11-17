import { test, expect } from "./fixtures.mjs";
import {
  listPath,
  rows,
  ready,
  openList,
  activate,
  listLink,
  align,
  openItem,
  restored,
} from "./history-helpers.mjs";

for (const style of ["preview", "headlines", "gallery"]) {
  test.describe(style, () => {
    test.beforeEach(async ({ page, reader }) => openList(page, reader, style));

    test("Back and Forward restore the same list position", async ({
      page,
      isMobile,
      browserName,
    }) => {
      const before = await openItem(page, isMobile);
      await page.goBack({ waitUntil: "commit" });
      await restored(page, before);
      if (browserName === "chromium")
        expect(
          await page.evaluate(() => window.readerTestCachedReturn),
          "This case must load a new document",
        ).toBe(false);
      await page.goForward({ waitUntil: "commit" });
      await expect(page.locator("#item-content-body")).toBeVisible();
      await page.goBack({ waitUntil: "commit" });
      await restored(page, before);
    });

    test("p returns to the same list position", async ({ page, isMobile }) => {
      const before = await openItem(page, isMobile);
      await page.keyboard.press("p");
      await restored(page, before);
    });

    test("list name resumes the originating list", async ({
      page,
      isMobile,
    }) => {
      const before = await openItem(page, isMobile);
      await activate(page, await listLink(page, "all", isMobile), isMobile);
      await restored(page, before);
    });

    test("browser Reload preserves position", async ({ page }) => {
      const before = await align(page);
      await page.reload();
      await restored(page, before);
    });

    test("explicit refresh starts at the top without adding history", async ({
      page,
      isMobile,
    }) => {
      test.fixme(
        isMobile,
        "The real mobile renderer has no visible explicit refresh control.",
      );
      await align(page);
      const length = await page.evaluate(() => history.length);
      await activate(
        page,
        page.locator(".btn-reload-current-view:visible"),
        isMobile,
      );
      await ready(page);
      await expect.poll(() => page.evaluate(() => scrollY)).toBe(0);
      expect(await page.evaluate(() => history.length)).toBe(length);
    });

    test("a different list starts at the top and Back restores the old list", async ({
      page,
      isMobile,
    }) => {
      const before = await align(page);
      await activate(page, await listLink(page, "saved", isMobile), isMobile);
      await ready(page);
      await expect.poll(() => page.evaluate(() => scrollY)).toBe(0);
      await page.goBack({ waitUntil: "commit" });
      await restored(page, before);
    });

    for (const action of ["reload", "next", "inspect"]) {
      test(`${action} article preserves the list origin`, async ({
        page,
        isMobile,
      }) => {
        const before = await openItem(page, isMobile);
        if (action === "reload") await page.reload();
        else if (action === "next") {
          await Promise.all([
            page.waitForEvent("load"),
            page.keyboard.press("n"),
          ]);
        } else {
          const menu = page.getByRole("button", {
            name: "More item actions",
            exact: true,
          });
          await (isMobile ? menu.tap() : menu.click());
          await activate(
            page,
            page
              .getByRole("link", { name: "Inspect item", exact: true })
              .first(),
            isMobile,
          );
        }
        await activate(page, await listLink(page, "all", isMobile), isMobile);
        await restored(page, before);
      });
    }
  });
}

test("reset restores database writes and stable demo identities", async ({
  page,
  reader,
  seed,
}) => {
  await openList(page, reader, "preview", "&filter=unread");
  const original = await page
    .locator(rows)
    .evaluateAll((elements) => elements.map((element) => element.id));
  const id = original[0].slice(5);
  const response = await page.request.post(
    `${reader.url}/reader/item/by-id/${id}/state`,
    { form: { action: "seen" } },
  );
  expect(response.ok()).toBeTruthy();
  await page.reload();
  await expect(page.locator(`#item-${id}`)).toHaveCount(0);
  await page.goto("about:blank");
  expect(await reader.reset()).toEqual(seed);
  await openList(page, reader, "preview", "&filter=unread");
  expect(
    await page
      .locator(rows)
      .evaluateAll((elements) => elements.map((element) => element.id)),
  ).toEqual(original);
});

for (const action of ["Back", "list name"]) {
  test(`${action} returns to the same real ranked batch`, async ({
    page,
    reader,
    isMobile,
  }) => {
    await page.goto(
      `${reader.url}${listPath}?list-style=preview&sort-order=ranked`,
    );
    await ready(page);
    await activate(
      page,
      page.locator(".btn-next-batch:visible").first(),
      isMobile,
    );
    await ready(page);
    expect(new URL(page.url()).searchParams.has("rank-id")).toBeTruthy();
    const ids = await page
      .locator(rows)
      .evaluateAll((elements) => elements.map((element) => element.id));
    const before = await openItem(page, isMobile, 4);
    if (action === "Back") await page.goBack({ waitUntil: "commit" });
    else await activate(page, await listLink(page, "all", isMobile), isMobile);
    await restored(page, before);
    expect(
      await page
        .locator(rows)
        .evaluateAll((elements) => elements.map((element) => element.id)),
    ).toEqual(ids);
  });
}

for (const where of ["list", "article", "article then Reload"]) {
  test(`Skip to content on ${where} preserves the origin`, async ({
    page,
    reader,
    isMobile,
  }) => {
    await openList(page, reader);
    async function skip() {
      const link = page.getByRole("link", {
        name: "Skip to content",
        exact: true,
      });
      await link.focus();
      await page.keyboard.press("Enter");
      await expect(page).toHaveURL(/#reader-main$/);
    }
    if (where === "list") await skip();
    const before = await openItem(page, isMobile);
    if (where !== "list") await skip();
    if (where === "article then Reload") await page.reload();
    await activate(page, await listLink(page, "all", isMobile), isMobile);
    await restored(page, before);
  });
}

test("blocked session storage retains native Back and Reload", async ({
  page,
  reader,
  isMobile,
}) => {
  await page.addInitScript(() =>
    Object.defineProperty(window, "sessionStorage", {
      get() {
        throw new DOMException("Storage blocked", "SecurityError");
      },
    }),
  );
  await openList(page, reader);
  const before = await openItem(page, isMobile);
  await page.goBack({ waitUntil: "commit" });
  await restored(page, before);
  await page.reload();
  await restored(page, before);
});

test("separate visits to the same URL retain separate positions", async ({
  page,
  reader,
  isMobile,
}) => {
  await openList(page, reader);
  const first = await openItem(page, isMobile, 8);
  await activate(page, await listLink(page, "all", isMobile), isMobile);
  await restored(page, first);
  const second = await openItem(page, isMobile, 14);
  expect(second.id).not.toBe(first.id);
  await page.goBack({ waitUntil: "commit" });
  await restored(page, second);
  await page.goBack({ waitUntil: "commit" });
  await expect(page.locator("#item-content-body")).toBeVisible();
  await page.goBack({ waitUntil: "commit" });
  await restored(page, first);
});

test("Reader home resumes through its canonical list link", async ({
  page,
  reader,
  isMobile,
}) => {
  await page.goto(`${reader.url}/reader/?list-style=preview`);
  await ready(page);
  const before = await openItem(page, isMobile);
  await activate(page, await listLink(page, "all", isMobile), isMobile);
  await restored(page, before);
});
