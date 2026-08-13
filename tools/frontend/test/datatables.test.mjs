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

async function addScript(page, ...parts) {
  await page.addScriptTag({ path: asset(...parts) });
}

function collectPageErrors(page) {
  const errors = [];
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  return errors;
}

test("Reader and Dashboard scripts run with the current frontend runtimes", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  await t.test("Reader initializes with jQuery 4", async () => {
    const page = await browser.newPage();
    const errors = collectPageErrors(page);
    await page.setContent(`
      <!doctype html>
      <html>
        <head>
          <meta name="llar-mode" content="list-items">
          <meta name="llar-id" content="">
          <meta name="llar-title" content="Reader smoke test">
        </head>
        <body class="llar-reader"><main></main></body>
      </html>
    `);
    await addScript(page, "jquery", "jquery.min.js");
    await page.evaluate(() => {
      window.jQuery.fn.popover = function () {
        return this;
      };
    });
    await addScript(page, "llar.js");
    await page.waitForFunction(() => window.jQuery?.fn?.jquery === "4.0.0");

    assert.deepEqual(errors, []);
    await page.close();
  });

  await t.test("Dashboard initializes and operates DataTables 3", async () => {
    const page = await browser.newPage();
    const errors = collectPageErrors(page);
    let sourceRequests = 0;

    await page.route("**/api/sources*", async (route) => {
      sourceRequests += 1;
      await route.fulfill({
        contentType: "application/json",
        body: JSON.stringify({
          data: [["example", "ok", "Example source", "now", "now", "1 item"]],
        }),
      });
    });
    await page.route("**/source-details/example", (route) =>
      route.fulfill({
        contentType: "text/html",
        body: "<p>Source details loaded</p>",
      }),
    );
    await page.route("http://llar.test/", (route) =>
      route.fulfill({
        contentType: "text/html",
        body: `
      <!doctype html>
      <html>
        <body>
          <div class="tab-pane active" data-tab-name="sources">
            <table id="sources-datatable" class="table">
              <thead><tr>
                <th>Source Key</th><th>Status</th><th>Source</th>
                <th>Last Success</th><th>Last Attempt</th><th>Stats</th><th>Actions</th>
              </tr></thead>
            </table>
          </div>
          <table class="datatable"><thead><tr><th>Name</th></tr></thead>
            <tbody><tr><td>Static row</td></tr></tbody></table>
          <table id="threads-datatable"><thead><tr>
            <th></th><th>Group</th><th>Name</th><th>State</th><th>Top frame</th>
          </tr></thead><tbody><tr data-stacktrace="Thread details">
            <td class="details-control">+</td><td>main</td><td>worker</td>
            <td>RUNNABLE</td><td>example.Frame</td>
          </tr></tbody></table>
        </body>
      </html>
      `,
      }),
    );
    await page.goto("http://llar.test/");
    await addScript(page, "jquery", "jquery.min.js");
    await addScript(page, "datatables", "dataTables.min.js");
    await addScript(page, "datatables", "dataTables.bootstrap5.min.js");
    await page.evaluate(() => {
      window.llarValueInspector = { mountAll() {} };
    });
    await addScript(page, "llar-status.js");

    await page.getByText("Example source").waitFor();
    assert.equal(await page.evaluate(() => window.jQuery.fn.jquery), "4.0.0");
    assert.equal(await page.evaluate(() => DataTable.version), "3.0.1");
    assert.equal(
      await page.evaluate(() => DataTable.isDataTable(".datatable")),
      true,
    );
    assert.equal(
      await page.evaluate(() => DataTable.isDataTable("#threads-datatable")),
      true,
    );
    await expectClass(page, "#sources-datatable tbody tr", "table-success");
    await page.locator("#sources-datatable .btn-source-details").click();
    await page.getByText("Source details loaded").waitFor();
    await page.locator("#threads-datatable td.details-control").click();
    await page.getByText("Thread details").waitFor();
    await Promise.all([
      page.waitForResponse((response) =>
        response.url().includes("/api/sources"),
      ),
      page.evaluate(() => reload_dashboard_tab(".tab-pane.active")),
    ]);

    assert.equal(sourceRequests, 2);
    assert.deepEqual(errors, []);
    await page.close();
  });
});

async function expectClass(page, selector, className) {
  const classes = await page.locator(selector).getAttribute("class");
  assert.ok(
    classes?.split(/\s+/).includes(className),
    `${selector} lacks ${className}`,
  );
}
