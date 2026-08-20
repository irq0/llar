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

function headlineRow(id, tags, title) {
  return `<tr id="item-${id}" class="reader-headline-row" data-id="${id}"
              data-item-tags='${JSON.stringify(tags)}' data-unread="true">
    <td class="reader-headline-select"><input class="form-check-input reader-headline-checkbox"
      type="checkbox" aria-label="Select ${title}"></td>
    <td class="reader-headline-marker"></td><th class="reader-headline-title">${title}</th>
  </tr>`;
}

test("Headline selection applies one explicit bulk tag edit", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());
  const page = await browser.newPage({
    viewport: { width: 1100, height: 800 },
  });
  await page.setContent(`<!doctype html><html><head>
    <meta name="llar-mode" content="list-items"></head><body class="llar-reader">
    <a href="#" class="btn-select-headlines">Select headlines</a>
    <main id="reader-main"><div id="headlines" class="reader-headlines">
      <section class="reader-bulk-selection-bar" hidden>
        <label><input class="reader-bulk-select-all" type="checkbox">Select all</label>
        <span class="reader-bulk-selection-count"></span>
        <button class="reader-bulk-edit-tags" type="button" disabled
          data-bs-toggle="modal" data-bs-target="#reader-bulk-tags-modal">Edit tags</button>
        <button class="reader-bulk-cancel" type="button">Cancel</button>
      </section>
      <table class="reader-headlines-table"><colgroup>
        <col class="reader-headline-select-column"><col>
      </colgroup><tbody>
        ${headlineRow(41, ["research", "later"], "First")}
        ${headlineRow(42, ["research"], "Second")}
      </tbody></table>
      <div class="modal reader-bulk-tags-modal" id="reader-bulk-tags-modal" tabindex="-1">
        <div class="modal-dialog"><form class="modal-content" id="reader-bulk-tags-form">
          <div class="modal-header"><span class="reader-bulk-tags-modal-count"></span></div>
          <div class="modal-body"><div class="reader-bulk-tag-context"></div>
            <div class="reader-bulk-tag-chips" data-operation="add"></div>
            <input class="reader-bulk-tag-input" data-operation="add">
            <div class="reader-bulk-tag-chips" data-operation="remove"></div>
            <input class="reader-bulk-tag-input" data-operation="remove">
            <div class="reader-bulk-tags-error" hidden></div>
          </div><div class="modal-footer">
            <button class="reader-bulk-tags-submit" type="submit" disabled>Apply tags</button>
          </div>
        </form></div>
      </div>
    </div><div id="reader-list-lifecycle-status" hidden></div></main>
    <div id="reader-global-status" hidden><span class="reader-global-status-message"></span></div>
  </body></html>`);
  await page.addStyleTag({
    path: asset("bootstrap", "css", "bootstrap.min.css"),
  });
  await page.addStyleTag({ path: asset("llar.css") });
  await page.addScriptTag({ path: asset("jquery", "jquery.min.js") });
  await page.addScriptTag({
    path: asset("bootstrap", "js", "bootstrap.bundle.min.js"),
  });
  await page.evaluate(() => {
    window.jQuery.fn.popover = function () {
      return this;
    };
    window.bulkTagRequest = window.jQuery.Deferred();
    window.jQuery.ajax = function (options) {
      window.bulkTagOptions = options;
      return window.bulkTagRequest.promise();
    };
  });
  await page.addScriptTag({ path: asset("llar.js") });
  await page.waitForTimeout(50);

  await page.locator(".btn-select-headlines").click();
  assert.equal(
    await page
      .locator("body")
      .evaluate((element) =>
        element.classList.contains("reader-bulk-selection-active"),
      ),
    true,
  );
  await page.locator(".reader-bulk-select-all").check();
  assert.equal(
    await page.locator(".reader-bulk-selection-count").textContent(),
    "2 selected",
  );

  await page.locator(".reader-bulk-edit-tags").click();
  await page.locator("#reader-bulk-tags-modal").waitFor({ state: "visible" });
  assert.match(
    await page.locator(".reader-bulk-tag-context").textContent(),
    /later · 1 of 2/,
  );
  await page.getByTitle("Add later to all").click();
  await page.getByTitle("Remove research from all").click();
  const addInput = page.locator('.reader-bulk-tag-input[data-operation="add"]');
  await addInput.fill("Project X");
  await addInput.press("Enter");
  await page.locator(".reader-bulk-tags-submit").click();

  const payload = await page.evaluate(() =>
    JSON.parse(window.bulkTagOptions.data),
  );
  assert.deepEqual(payload, {
    item_ids: [41, 42],
    add_tags: ["later", "project-x"],
    remove_tags: ["research"],
  });
  await page.evaluate(() => {
    window.bulkTagRequest.resolve({
      items: [
        { id: 41, unread: true, "item-tags": ["later", "project-x"] },
        { id: 42, unread: true, "item-tags": ["later", "project-x"] },
      ],
    });
  });
  await page.waitForFunction(
    () => !document.body.classList.contains("reader-bulk-selection-active"),
  );
  assert.equal(
    await page.locator(".reader-bulk-selection-bar").isHidden(),
    true,
  );
  assert.deepEqual(
    await page
      .locator("#item-41")
      .evaluate((element) =>
        JSON.parse(element.getAttribute("data-item-tags")),
      ),
    ["later", "project-x"],
  );

  await page.setViewportSize({ width: 500, height: 800 });
  assert.equal(await page.locator(".btn-select-headlines").isVisible(), false);
});
