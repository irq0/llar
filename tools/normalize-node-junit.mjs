import { readFile, writeFile } from "node:fs/promises";

const [reportPath, suiteName] = process.argv.slice(2);

if (!reportPath || !suiteName) {
  throw new Error("usage: normalize-node-junit.mjs REPORT_PATH SUITE_NAME");
}

const rawXml = await readFile(reportPath, "utf8");
// Test failures can contain ANSI escape sequences. Node's JUnit reporter leaves
// their control bytes intact even though XML 1.0 does not permit them.
const xml = rawXml
  .replace(/\u001b\[[0-?]*[ -/]*[@-~]/g, "")
  .replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f]/g, "");
const documentMatch = xml.match(
  /^\s*<\?xml[^>]*>\s*<testsuites>\s*([\s\S]*?)\s*<\/testsuites>\s*$/,
);

if (!documentMatch) {
  throw new Error(`unexpected Node.js JUnit structure in ${reportPath}`);
}

const body = documentMatch[1];

// Node.js currently writes testcase elements directly below testsuites. Most
// JUnit consumers, including our GitHub publisher, expect a testsuite layer.
if (/^\s*<testsuite(?:\s|>)/.test(body)) {
  if (xml !== rawXml) {
    await writeFile(reportPath, xml);
  }
} else {
  const counts = new Map(
    [
      ...xml.matchAll(
        /<!--\s*(tests|fail|cancelled|skipped|todo)\s+(\d+)\s*-->/g,
      ),
    ].map(([, name, value]) => [name, Number(value)]),
  );
  const durationMatch = xml.match(/<!--\s*duration_ms\s+([\d.]+)\s*-->/);
  const escapedSuiteName = suiteName
    .replaceAll("&", "&amp;")
    .replaceAll('"', "&quot;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;");
  const attributes = [
    `name="${escapedSuiteName}"`,
    `tests="${counts.get("tests") ?? 0}"`,
    `failures="${counts.get("fail") ?? 0}"`,
    `errors="${counts.get("cancelled") ?? 0}"`,
    `skipped="${(counts.get("skipped") ?? 0) + (counts.get("todo") ?? 0)}"`,
    `time="${durationMatch ? Number(durationMatch[1]) / 1000 : 0}"`,
  ].join(" ");

  await writeFile(
    reportPath,
    `<?xml version="1.0" encoding="utf-8"?>\n<testsuites>\n<testsuite ${attributes}>\n${body}\n</testsuite>\n</testsuites>\n`,
  );
}
