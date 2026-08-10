const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const test = require("node:test");
const { JSDOM } = require("jsdom");

const inspectorSource = fs.readFileSync(
  path.join(__dirname, "../../../resources/status/llar-value-inspector.js"),
  "utf8",
);

function scalar(printed, semanticType, runtimeType, extra) {
  return Object.assign(
    {
      kind: "scalar",
      "semantic-type": semanticType,
      "runtime-type": runtimeType,
      printed,
    },
    extra || {},
  );
}

function keyword(name) {
  return scalar(":" + name, "keyword", "clojure.lang.Keyword");
}

function map(entries) {
  return {
    kind: "map",
    "semantic-type": "map",
    "runtime-type": "clojure.lang.PersistentArrayMap",
    open: "{",
    close: "}",
    count: entries.length,
    "count-known": true,
    children: entries.map(([key, value]) => ({ key, value })),
  };
}

function vector(children) {
  return {
    kind: "collection",
    "semantic-type": "vector",
    "runtime-type": "clojure.lang.PersistentVector",
    open: "[",
    close: "]",
    count: children.length,
    "count-known": true,
    children,
  };
}

function setup(node, label = "Value", options) {
  const dom = new JSDOM("<div id=app></div>", {
    runScripts: "dangerously",
    url: "https://llar.test/",
  });
  dom.window.eval(inspectorSource);
  const container = dom.window.document.querySelector("#app");
  const payload = { roots: [{ id: "value", label, node }] };
  const state = dom.window.llarValueInspector.mount(
    container,
    payload,
    options,
  );
  return { dom, container, payload, state };
}

function delay(milliseconds) {
  return new Promise((resolve) => setTimeout(resolve, milliseconds));
}

test("compact mode renders immediately without inspector chrome", () => {
  const root = map([
    [keyword("status"), scalar(":running", "keyword", "clojure.lang.Keyword")],
    [
      keyword("details"),
      map([[keyword("count"), scalar("3", "integer", "java.lang.Long")]]),
    ],
  ]);
  const { container, state } = setup(root, "State", { variant: "compact" });

  assert.ok(container.classList.contains("clojure-value-inspector-compact"));
  assert.equal(container.querySelector(".clojure-inspector-toolbar"), null);
  assert.equal(container.querySelector(".clojure-inspector-selection"), null);
  assert.equal(container.textContent.includes("Inspect value"), false);
  assert.match(container.textContent, /:status/);
  assert.match(container.textContent, /:count/);
  assert.equal(state.showTypes, false);
});

test("filters render an explicit empty state and the value copy action is absent", async () => {
  const { container, state } = setup(
    map([[keyword("answer"), scalar("42", "integer", "java.lang.Long")]]),
  );

  assert.equal(container.textContent.includes("Copy EDN"), false);
  assert.equal(
    state.rootSelect.getAttribute("aria-label"),
    "Inspected value root",
  );
  assert.equal(
    state.viewSelect.getAttribute("aria-label"),
    "Value visualization",
  );

  state.filter.value = "missing";
  state.filter.dispatchEvent(
    new state.container.ownerDocument.defaultView.Event("input", {
      bubbles: true,
    }),
  );
  await delay(130);
  assert.match(
    container.querySelector(".clojure-inspector-empty").textContent,
    /No values match/,
  );
});

test("table mode is limited to regular scalar-keyed maps and has accessible headers", () => {
  const rows = vector([
    map([
      [keyword("id"), scalar("1", "integer", "java.lang.Long")],
      [keyword("name"), scalar('"Ada"', "string", "java.lang.String")],
    ]),
    map([
      [keyword("name"), scalar('"Grace"', "string", "java.lang.String")],
      [keyword("id"), scalar("2", "integer", "java.lang.Long")],
    ]),
  ]);
  const { container, state } = setup(rows, "People");
  assert.equal(state.viewSelect.options[1].disabled, false);

  state.viewSelect.value = "table";
  state.viewSelect.dispatchEvent(
    new container.ownerDocument.defaultView.Event("change"),
  );
  assert.equal(container.querySelector("caption").textContent, "People");
  assert.deepEqual(
    Array.from(container.querySelectorAll("thead th")).map(
      (cell) => cell.scope,
    ),
    ["col", "col", "col"],
  );
  assert.deepEqual(
    Array.from(container.querySelectorAll("tbody th")).map(
      (cell) => cell.scope,
    ),
    ["row", "row"],
  );

  const irregular = vector([
    map([[keyword("id"), scalar("1", "integer", "java.lang.Long")]]),
    map([[keyword("name"), scalar('"Ada"', "string", "java.lang.String")]]),
  ]);
  assert.equal(
    container.ownerDocument.defaultView.llarValueInspector.tableSchema(
      irregular,
    ),
    null,
  );
});

test("Clojure subtrees and get-in paths retain typed keys", () => {
  const root = map([
    [
      keyword("fields"),
      map([[keyword("title"), scalar('"LLAR"', "string", "java.lang.String")]]),
    ],
  ]);
  const { container, dom, state } = setup(root);
  const title = dom.window.llarValueInspector.nodeAtPath(root, [
    ":fields",
    ":title",
  ]);
  assert.equal(title["runtime-type"], "java.lang.String");

  const fields = container.querySelectorAll("details")[1];
  fields.open = true;
  fields.dispatchEvent(new dom.window.Event("toggle"));
  const value = container.querySelector(
    ".clojure-inspector-leaf .clojure-inspector-value .clojure-inspector-inline",
  );
  value.click();
  assert.equal(
    dom.window.llarValueInspector.pathToEdn(state.selectedPath),
    "[:fields :title]",
  );
});

test("remounting preserves per-root filter, view, and type preferences", () => {
  const rows = vector([
    map([[keyword("id"), scalar("1", "integer", "java.lang.Long")]]),
  ]);
  const { container, payload, state, dom } = setup(rows);
  state.viewSelect.value = "table";
  state.viewSelect.dispatchEvent(new dom.window.Event("change"));
  state.filter.value = "1";
  state.filter.dispatchEvent(new dom.window.Event("input", { bubbles: true }));
  const types = container.querySelector(
    ".clojure-inspector-types-toggle input",
  );
  types.click();

  const remounted = dom.window.llarValueInspector.mount(container, payload);
  assert.equal(remounted.viewSelect.value, "table");
  assert.equal(remounted.filter.value, "1");
  assert.equal(remounted.showTypes, false);
});

test("long and truncated strings disclose their size and truncation reason", () => {
  const long = scalar(
    '"' + "x".repeat(200) + '…"',
    "string",
    "java.lang.String",
    {
      length: 500,
      "captured-length": 200,
      truncated: true,
      "truncation-reason": "string-length",
    },
  );
  const { container } = setup(vector([long]));
  assert.match(container.textContent, /200 of 500 chars/);
  assert.match(container.textContent, /truncated/);
  assert.ok(container.querySelector(".clojure-inspector-long-value"));
});
