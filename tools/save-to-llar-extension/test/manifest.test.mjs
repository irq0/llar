import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const extensionRoot = new URL("../", import.meta.url);
const manifest = JSON.parse(
  await readFile(new URL("manifest.json", extensionRoot), "utf8"),
);

test("the manifest declares Chrome and Firefox background runtimes", () => {
  assert.equal(manifest.manifest_version, 3);
  assert.equal(manifest.minimum_chrome_version, "121");
  assert.deepEqual(manifest.background, {
    scripts: ["background.mjs"],
    service_worker: "background.mjs",
    type: "module",
  });
  assert.equal(
    manifest.browser_specific_settings.gecko.strict_min_version,
    "140.0",
  );
  assert.deepEqual(
    manifest.browser_specific_settings.gecko.data_collection_permissions,
    { required: ["authenticationInfo", "websiteActivity"] },
  );
  assert.match(
    manifest.browser_specific_settings.gecko.id,
    /^(?:\{[0-9a-f-]+\}|[^@]+@[^@]+)$/i,
  );
});

test("every local file referenced by the manifest exists", async () => {
  const referencedFiles = [
    manifest.background.service_worker,
    ...manifest.background.scripts,
    manifest.options_page,
    ...Object.values(manifest.icons),
    ...Object.values(manifest.action.default_icon),
  ];

  await Promise.all(
    referencedFiles.map((path) =>
      assert.doesNotReject(readFile(new URL(path, extensionRoot))),
    ),
  );
});
