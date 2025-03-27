#!/usr/bin/env node

const fs = require("fs");
const { Readability } = require("@mozilla/readability");
const createDOMPurify = require("dompurify");
const { JSDOM } = require("jsdom");
const window = new JSDOM("").window;
const DOMPurify = createDOMPurify(window);

function extract(html, url) {
  const clean = DOMPurify.sanitize(html);
  const dom = new JSDOM(clean, { url });
  const document = dom.window.document;
  const article = new Readability(document).parse();
  return article;
}

let raw_input = "";
process.stdin.setEncoding("utf-8");
process.stdin.on("data", (chunk) => {
  raw_input += chunk;
});
process.stdin.on("end", () => {
  let data;
  try {
    data = JSON.parse(raw_input);
  } catch (e) {
    console.error(JSON.stringify({ error: "invalid input JSON" }, null, 2));
    process.exit(2);
  }
  const { html, url } = data;
  const article = extract(html, url);
  if (article) {
    console.log(JSON.stringify(article, null, 2));
  } else {
    console.log(
      JSON.stringify(
        {
          error: "document-not-readerable",
        },
        null,
        2,
      ),
    );
    process.exit(1);
  }
});
