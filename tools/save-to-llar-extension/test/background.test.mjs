import assert from "node:assert/strict";
import test from "node:test";

const token = "0123456789abcdef".repeat(4);
let clickListener;
let storedSettings = {};
let openedOptions = 0;
const tabUpdates = [];

globalThis.browser = {
  action: {
    onClicked: {
      addListener(listener) {
        clickListener = listener;
      },
    },
    async setBadgeBackgroundColor() {},
    async setBadgeText() {},
    async setTitle() {},
  },
  runtime: {
    async openOptionsPage() {
      openedOptions += 1;
    },
  },
  storage: {
    local: {
      async get() {
        return storedSettings;
      },
    },
  },
  tabs: {
    async update(tabId, update) {
      tabUpdates.push({ tabId, update });
    },
  },
};

await import("../background.mjs");

test("a Firefox toolbar click opens settings when configuration is missing", async () => {
  storedSettings = {};
  openedOptions = 0;
  tabUpdates.length = 0;

  await clickListener({ id: 4, url: "https://example.org/article" });

  assert.equal(openedOptions, 1);
  assert.deepEqual(tabUpdates, []);
});

test("a Firefox toolbar click navigates to LLAR's capture feedback URL", async () => {
  storedSettings = {
    captureBase: "https://save.example.org/llar/",
    token,
  };
  openedOptions = 0;
  tabUpdates.length = 0;

  await clickListener({ id: 7, url: "https://example.org/article?id=3#notes" });

  assert.equal(openedOptions, 0);
  assert.deepEqual(tabUpdates, [
    {
      tabId: 7,
      update: {
        url:
          "https://save.example.org/llar/#" +
          token +
          ":https://example.org/article?id=3#notes",
      },
    },
  ]);
});

delete globalThis.browser;
