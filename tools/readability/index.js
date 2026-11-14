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

async function extract(html, url, { selector, from } = {}) {
  if (typeof html !== "string" || (url != null && typeof url !== "string")) {
    throw new TypeError("html must be a string and url a string or null");
  }

  // Cheerio and JSDOM are inert here: neither executes scripts nor loads page
  // resources. Only the extracted content crosses LLAR's rendering boundary.
  const metadata = url ? await scrapeMetadata({ url, html }) : {};
  const dom = new JSDOM(html, url ? { url } : {});
  const document = dom.window.document;
  // Keep selector structure and lazy-image attributes, but no executable page content.
  const snapshot = document.documentElement.cloneNode(true);
  for (const node of snapshot.querySelectorAll("[style]")) {
    if (node.style.display === "none" || node.style.visibility === "hidden")
      node.hidden = true;
  }
  const sourceSnapshot = DOMPurify.sanitize(snapshot.outerHTML, {
    ...RENDER_SANITIZE_CONFIG,
    WHOLE_DOCUMENT: true,
    ADD_TAGS: ["title"],
    ALLOW_DATA_ATTR: true,
  });
  if (selector) {
    const selected = document.querySelector(selector);
    if (!selected) return {};
    document.body.replaceChildren(selected.cloneNode(true));
    document.body
      .querySelectorAll("[hidden], [aria-hidden='true'], [style]")
      .forEach((node) => {
        if (
          node.hidden ||
          node.getAttribute("aria-hidden") === "true" ||
          node.style.display === "none" ||
          node.style.visibility === "hidden"
        )
          node.remove();
      });
    // The config selected the article; email layout classes must not score it as navigation.
    document.body
      .querySelectorAll("[class]")
      .forEach((node) => node.removeAttribute("class"));
    for (const table of [
      ...document.body.querySelectorAll("table"),
    ].reverse()) {
      if (
        !table.textContent.trim() &&
        table.querySelector("img") &&
        !table.querySelector("th, caption")
      ) {
        for (const node of [
          ...table.querySelectorAll("tbody, thead, tfoot, tr, td"),
        ].reverse()) {
          node.replaceWith(...node.childNodes);
        }
        table.replaceWith(...table.childNodes);
      }
    }
    for (const node of [...document.body.querySelectorAll("div")].reverse()) {
      // Readability can discard a captioned figure when it sits in a layout div.
      if (
        !node.id &&
        node.children.length === 1 &&
        node.firstElementChild.tagName === "FIGURE"
      ) {
        node.replaceWith(...node.childNodes);
      }
    }
  }
  // Stored email images already point at LLAR; Readability must not rebase these onto the publisher.
  const blobUrls = new Map();
  if (from === "contents" && url) {
    for (const image of document.querySelectorAll('img[src^="/blob/"]')) {
      const local = image.getAttribute("src");
      blobUrls.set(new URL(local, url).href, local);
    }
  }
  const article = new Readability(dom.window.document).parse() || {};
  if (article.content) {
    const content = DOMPurify.sanitize(article.content, {
      ...RENDER_SANITIZE_CONFIG,
      RETURN_DOM: true,
    });
    for (const image of content.querySelectorAll("img[src]")) {
      const local = blobUrls.get(image.getAttribute("src"));
      if (local) image.setAttribute("src", local);
    }
    article.content = content.innerHTML;
    article.sourceSnapshot = sourceSnapshot;
    article.sourceSnapshotUrl = /^https?:/.test(document.baseURI)
      ? document.baseURI
      : null;
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
    const article = await extract(data.html, data.url, {
      selector: data.selector,
      from: data.from,
    });
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
