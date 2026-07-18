#!/usr/bin/env node

const fs = require("fs");
const { Readability } = require("@mozilla/readability");
const createDOMPurify = require("dompurify");
const { JSDOM } = require("jsdom");

const window = new JSDOM("").window;
const DOMPurify = createDOMPurify(window);

// These plugins only inspect the supplied document. Network-backed media and
// iframe/oEmbed plugins must not run during article fetches.
const scrapeMetadata = require("metascraper")([
  require("metascraper-author")(),
  require("metascraper-date")(),
  require("metascraper-description")(),
  require("metascraper-image")(),
  require("metascraper-logo")(),
  require("metascraper-publisher")(),
  require("metascraper-title")(),
  require("metascraper-url")(),
  require("metascraper-amazon")(),
]);

const RENDER_SANITIZE_CONFIG = {
  USE_PROFILES: { html: true },
  ALLOW_DATA_ATTR: false,
  FORBID_TAGS: [
    "button",
    "form",
    "input",
    "option",
    "select",
    "style",
    "template",
    "textarea",
  ],
  FORBID_ATTR: ["autoplay", "style"],
};

async function extract(html, url) {
  if (typeof html !== "string" || typeof url !== "string") {
    throw new TypeError("html and url must be strings");
  }

  // Cheerio and JSDOM are inert here: neither executes scripts nor loads page
  // resources. Only the extracted content crosses LLAR's rendering boundary.
  const metadata = await scrapeMetadata({ url, html });
  const dom = new JSDOM(html, { url });
  const article = new Readability(dom.window.document).parse() || {};
  if (article.content) {
    article.content = DOMPurify.sanitize(
      article.content,
      RENDER_SANITIZE_CONFIG,
    );
  }
  article.metadata = metadata;
  return article;
}

async function main() {
  let data;
  try {
    data = JSON.parse(fs.readFileSync(0, "utf8"));
  } catch (_) {
    console.error(JSON.stringify({ error: "invalid input JSON" }, null, 2));
    process.exitCode = 2;
    return;
  }

  try {
    const article = await extract(data.html, data.url);
    console.log(JSON.stringify(article, null, 2));
  } catch (error) {
    console.error(error?.stack || String(error));
    process.exitCode = 1;
  }
}

if (require.main === module) {
  main();
}

module.exports = {
  extract,
};
