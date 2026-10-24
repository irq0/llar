import assert from "node:assert/strict";
import test from "node:test";

import {
  getExtensionApi,
  restrictStorageToExtension,
} from "../extension-api.mjs";

test("Firefox's browser API is preferred when it is available", () => {
  const browser = {};
  const chrome = {};
  assert.equal(getExtensionApi({ browser, chrome }), browser);
});

test("the Chrome API is used as the cross-browser fallback", () => {
  const chrome = {};
  assert.equal(getExtensionApi({ chrome }), chrome);
  assert.throws(() => getExtensionApi({}), /WebExtensions API is unavailable/);
});

test("Chrome storage is restricted to extension contexts", async () => {
  let receivedOptions;
  const local = {
    async setAccessLevel(options) {
      assert.equal(this, local);
      receivedOptions = options;
    },
  };

  await restrictStorageToExtension({ storage: { local } });
  assert.deepEqual(receivedOptions, { accessLevel: "TRUSTED_CONTEXTS" });
});

test("Firefox works without Chrome's storage access-level API", async () => {
  await assert.doesNotReject(
    restrictStorageToExtension({ storage: { local: {} } }),
  );
});
