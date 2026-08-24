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
    let podcastActionRequests = 0;

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
    await page.route("**/api/podcast/retry/42", (route) => {
      podcastActionRequests += 1;
      return route.fulfill({
        contentType: "application/json",
        body: JSON.stringify({ itemId: 42, status: "pending" }),
      });
    });
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
          <table id="bookmarks-datatable"><thead><tr>
            <th>Status</th><th>URL / title</th><th>Submitted</th><th>Attempts</th>
            <th>Last attempt</th><th>Error</th><th>Actions</th>
          </tr></thead><tbody>
            <tr><td>ready</td><td>Zulu capture</td><td>phone</td><td data-order="2">2</td>
              <td data-order="20">later</td><td></td><td><button>Dismiss Zulu</button></td></tr>
            <tr><td>failed</td><td>Alpha capture</td><td>browser</td><td data-order="10">10</td>
              <td data-order="10">earlier</td><td>network failed</td><td><button>Retry Alpha</button></td></tr>
          </tbody></table>
          <table id="podcasts-datatable"><thead><tr>
            <th>Status</th><th>Item</th><th>Source</th><th>Title / URL</th>
            <th>Duration</th><th>Size</th><th>Downloaded</th><th>Last Attempt</th>
            <th>Error</th><th>Actions</th>
          </tr></thead><tbody>
            <tr><td>complete</td><td data-order="42">42</td><td>demo</td><td>Episode Alpha</td>
              <td data-order="90">00:01:30</td><td data-order="1024">1 KiB</td>
              <td data-order="30">downloaded</td><td data-order="20">later</td><td></td>
              <td><button type="button" class="btn-podcast-action"
                data-endpoint="/api/podcast/retry/42" data-method="POST">Delete episode</button></td></tr>
          </tbody></table>
          <table id="states-datatable"><thead><tr>
            <th>State</th><th>Running?</th><th>Type</th>
          </tr></thead><tbody>
            <tr class="state-row" data-state-key="alpha">
              <td>#'llar.test/alpha</td><td data-order="1">true</td><td>Map</td>
            </tr>
            <tr class="state-detail-row" data-state-key="alpha"><td colspan="3">
              <div data-clojure-inspector>Alpha inspector secret</div>
            </td></tr>
            <tr class="state-row" data-state-key="beta">
              <td>#'llar.test/beta</td><td data-order="0">false</td><td>Vector</td>
            </tr>
            <tr class="state-detail-row" data-state-key="beta"><td colspan="3">
              <div data-clojure-inspector>Beta inspector secret</div>
            </td></tr>
          </tbody></table>
          <table id="schedules-datatable"><thead><tr>
            <th>Name</th><th>State</th><th>Type</th><th>Canned</th><th>Next Run</th>
            <th>Running?</th><th>Last Started</th><th>Last Finished</th>
            <th>Duration</th><th>Trigger</th><th>Actions</th>
          </tr></thead><tbody>
            <tr class="schedule-row" data-schedule-key="alpha">
              <td>Alpha schedule</td><td>started</td><td>defsched</td><td>hourly</td>
              <td data-order="10">soon</td><td data-order="0">no</td>
              <td data-order="10">earlier</td><td data-order="20">later</td>
              <td data-order="1000">1 s</td><td>manual</td><td><button>Run Alpha</button></td>
            </tr>
            <tr class="schedule-detail-row" data-schedule-key="alpha"><td colspan="11">
              <div>Alpha schedule detail</div>
            </td></tr>
            <tr class="schedule-row" data-schedule-key="beta">
              <td>Beta schedule</td><td>started</td><td>defsched</td><td>daily</td>
              <td data-order="20">later</td><td data-order="1">yes</td>
              <td data-order="20">later</td><td data-order="30">latest</td>
              <td data-order="2000">2 s</td><td>timer</td><td><button>Run Beta</button></td>
            </tr>
            <tr class="schedule-detail-row" data-schedule-key="beta"><td colspan="11">
              <div>Beta schedule detail</div>
            </td></tr>
          </tbody></table>
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
    assert.equal(
      await page.evaluate(() => DataTable.isDataTable(".datatable")),
      true,
    );
    assert.equal(
      await page.evaluate(() => DataTable.isDataTable("#threads-datatable")),
      true,
    );
    for (const selector of [
      "#bookmarks-datatable",
      "#podcasts-datatable",
      "#states-datatable",
      "#schedules-datatable",
    ]) {
      assert.equal(
        await page.evaluate((table) => DataTable.isDataTable(table), selector),
        true,
      );
      assert.equal(await tableHasPagingControls(page, selector), false);
    }
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

    assert.match(
      await page.locator("#bookmarks-datatable tbody tr").first().innerText(),
      /Zulu capture/,
    );
    assert.equal(
      await page.evaluate(() => {
        const table = datatable_for("#bookmarks-datatable");
        table.search("Alpha capture").draw();
        return table.rows({ search: "applied" }).count();
      }),
      1,
    );
    assert.equal(
      await page.locator("#bookmarks-datatable button").innerText(),
      "Retry Alpha",
    );
    await page.evaluate(() =>
      datatable_for("#bookmarks-datatable").search("").draw(),
    );

    assert.equal(
      await page.evaluate(() => {
        const table = datatable_for("#states-datatable");
        table.search("inspector secret").draw();
        const count = table.rows({ search: "applied" }).count();
        table.search("").draw();
        return count;
      }),
      0,
    );
    assert.equal(
      await page.locator("#states-datatable [data-clojure-inspector]").count(),
      2,
    );

    const stateOrder = await page.evaluate(() => {
      const table = datatable_for("#states-datatable");
      table.order([0, "desc"]).draw();
      const row = document.querySelector(
        "#states-datatable tbody tr.state-row",
      );
      const detail = row.nextElementSibling;
      return {
        state: row.dataset.stateKey,
        childClass: detail.className,
        detailKey: detail.querySelector(".state-detail-content")?.dataset
          .stateKey,
        detailText: detail.textContent,
      };
    });
    assert.equal(stateOrder.state, "beta");
    assert.match(stateOrder.childClass, /state-detail-child/);
    assert.equal(stateOrder.detailKey, "beta");
    assert.match(stateOrder.detailText, /Beta inspector secret/);

    const scheduleOrder = await page.evaluate(() => {
      const table = datatable_for("#schedules-datatable");
      table.order([0, "desc"]).draw();
      const row = document.querySelector(
        "#schedules-datatable tbody tr.schedule-row",
      );
      const detail = row.nextElementSibling;
      return {
        schedule: row.dataset.scheduleKey,
        childClass: detail.className,
        detailKey: detail.querySelector(".schedule-detail-content")?.dataset
          .scheduleKey,
        detailText: detail.textContent,
      };
    });
    assert.equal(scheduleOrder.schedule, "beta");
    assert.match(scheduleOrder.childClass, /schedule-detail-child/);
    assert.equal(scheduleOrder.detailKey, "beta");
    assert.match(scheduleOrder.detailText, /Beta schedule detail/);

    assert.equal(
      await page.locator("#podcasts-datatable button").innerText(),
      "Delete episode",
    );
    assert.deepEqual(
      await page.evaluate(() => datatable_for("#podcasts-datatable").order()),
      [
        [0, "asc"],
        [6, "desc"],
      ],
    );
    await page.evaluate(() => {
      window.podcastTabReloaded = false;
      window.reload_dashboard_tab = function () {
        window.podcastTabReloaded = true;
      };
    });
    await page.locator("#podcasts-datatable .btn-podcast-action").click();
    await page.waitForFunction(() => window.podcastTabReloaded);
    assert.equal(page.url(), "http://llar.test/");
    assert.equal(podcastActionRequests, 1);
    assert.equal(
      await actionColumnIsOrderable(page, "#bookmarks-datatable", 6),
      false,
    );
    assert.equal(
      await actionColumnIsOrderable(page, "#podcasts-datatable", 9),
      false,
    );
    assert.equal(
      await actionColumnIsOrderable(page, "#schedules-datatable", 10),
      false,
    );

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

async function tableHasPagingControls(page, selector) {
  return page.evaluate((tableSelector) => {
    const table = document.querySelector(tableSelector);
    return Boolean(
      table.closest(".dt-container")?.querySelector(".dt-paging, .dt-length"),
    );
  }, selector);
}

async function actionColumnIsOrderable(page, selector, column) {
  return page.evaluate(
    ({ tableSelector, columnIndex }) => {
      const header = datatable_for(tableSelector).column(columnIndex).header();
      return header.classList.contains("dt-orderable-asc");
    },
    { tableSelector: selector, columnIndex: column },
  );
}
