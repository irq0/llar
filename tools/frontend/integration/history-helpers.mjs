import { expect } from "@playwright/test";

export const listPath = "/reader/group/default/all/source/all/items";
export const rows = '[id^="item-"][data-id]';

export async function ready(page) {
  await expect(page.locator("body")).toHaveClass(/reader-mode-list-items/);
  await page.evaluate(async () => {
    await document.fonts.ready;
    await Promise.all(
      [...document.images]
        .filter((image) => image.loading !== "lazy")
        .map((image) =>
          image.complete
            ? undefined
            : new Promise((resolve) => {
                image.addEventListener("load", resolve, { once: true });
                image.addEventListener("error", resolve, { once: true });
              }),
        ),
    );
    await new Promise((resolve) =>
      requestAnimationFrame(() => requestAnimationFrame(resolve)),
    );
  });
}

export async function openList(page, reader, style = "preview", query = "") {
  const response = await page.goto(
    `${reader.url}${listPath}?list-style=${style}&sort-order=newest${query}`,
  );
  expect(response.status()).toBe(200);
  expect(response.headers()["referrer-policy"]).toBe("no-referrer");
  expect(response.headers()["content-security-policy"]).toContain(
    "script-src 'self'",
  );
  await ready(page);
}

export async function activate(page, locator, isMobile) {
  await Promise.all([
    page.waitForEvent("load"),
    isMobile ? locator.tap() : locator.click(),
  ]);
}

export async function listLink(page, name, isMobile) {
  if (!isMobile && name === "all")
    return page
      .locator(".reader-navbar-path")
      .getByRole("link", { name, exact: true });
  if (isMobile) {
    await page
      .getByRole("button", { name: "Open Reader navigation", exact: true })
      .tap();
    await expect(page.locator("body")).toHaveClass(
      /reader-mobile-navigation-open/,
    );
  }
  // The filter list precedes Favorites, which also contains an "all" destination.
  return page
    .locator("#groupnav")
    .getByRole("link", { name, exact: true })
    .first();
}

export async function align(page, index = 12) {
  const row = page.locator(rows).nth(index);
  const id = await row.getAttribute("id");
  await row.evaluate((element) =>
    scrollBy({
      top:
        element.getBoundingClientRect().top -
        document.getElementById("top-nav").getBoundingClientRect().bottom -
        24,
      behavior: "instant",
    }),
  );
  const before = await row.evaluate((element) => ({
    id: element.id,
    url: location.href,
    top: element.getBoundingClientRect().top,
    scrollY,
  }));
  expect(
    before.scrollY,
    "The starting list must actually scroll",
  ).toBeGreaterThan(8);
  return { ...before, id };
}

export async function openItem(page, isMobile, index = 12) {
  const before = await align(page, index);
  // WebKit can scroll to expose the title before tapping; measure at actual departure.
  const [message] = await Promise.all([
    page.waitForEvent("console", {
      predicate: (entry) => entry.text().startsWith("LLAR_BROWSER_DEPARTURE "),
    }),
    activate(
      page,
      page.locator(`#${before.id} a[href*="/item/by-id/"]`).first(),
      isMobile,
    ),
  ]);
  const departure = JSON.parse(
    message.text().slice("LLAR_BROWSER_DEPARTURE ".length),
  );
  expect(departure.id).toBe(before.id);
  expect(departure.scrollY).toBeGreaterThan(8);
  await expect(page.locator("#item-content-body")).toBeVisible();
  return departure;
}

export async function restored(page, before) {
  await ready(page);
  await expect(page).toHaveURL(before.url);
  await expect
    .poll(
      async () =>
        Math.abs(
          (await page
            .locator(`#${before.id}`)
            .evaluate((element) => element.getBoundingClientRect().top)) -
            before.top,
        ),
      { message: `Restore ${before.id} within 8 px of ${before.top}` },
    )
    .toBeLessThanOrEqual(8);
}
