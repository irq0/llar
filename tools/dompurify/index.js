#!/usr/bin/env node

const createDOMPurify = require("dompurify");
const { JSDOM } = require("jsdom");
const window = new JSDOM("").window;
const DOMPurify = createDOMPurify(window);

let raw_input = "";
process.stdin.setEncoding("utf-8");
process.stdin.on("data", (chunk) => {
  raw_input += chunk;
});
process.stdin.on("end", () => {
  const clean = DOMPurify.sanitize(raw_input);
  console.log(clean);
});
