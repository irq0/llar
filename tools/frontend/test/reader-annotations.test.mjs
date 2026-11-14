import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { chromium } from "playwright";

test("highlights element-boundary selections and reanchors after email extraction", async (t) => {
  const browser = await chromium.launch(
    process.env.LLAR_BROWSER_EXECUTABLE
      ? { executablePath: process.env.LLAR_BROWSER_EXECUTABLE, headless: true }
      : {
          channel: process.env.LLAR_BROWSER_CHANNEL ?? "chrome",
          headless: true,
        },
  );
  t.after(() => browser.close());
  const page = await browser.newPage();
  await page.setContent(`<div id="item-content-body"><span hidden>Inbox preview</span>
    <table><tr><td><p>Read <strong>this passage</strong> carefully.</p></td></tr></table></div>
    <div id="annotation-selection-actions"></div><div id="annotation-mode-status"></div>
    <div id="annotation-highlight-list"></div>`);
  await page.addScriptTag({
    path: path.resolve("resources/status/jquery/jquery.min.js"),
  });
  await page.addScriptTag({ path: path.resolve("resources/status/llar.js") });
  const result = await page.evaluate(() => {
    const container = document.querySelector("#item-content-body");
    const range = document.createRange();
    range.selectNodeContents(container.querySelector("p"));
    window.getSelection().addRange(range);
    window.onTextSelected();
    const selector = window.pendingSelector;
    window.annotations = [{ id: 1, selector }];
    window.getSelection().removeAllRanges();
    container.innerHTML =
      "<h2>Extracted article</h2><p>Read <strong>this passage</strong> carefully.</p>";
    window.renderHighlights();
    return {
      selector,
      reanchored: window.highlightRanges.get(1)?.toString(),
      emptyQuote: window.findByQuoteSelector(container, { exact: "" }),
      blankQuote: window.findByQuoteSelector(container, { exact: "  " }),
      missingQuote: window.findByQuoteSelector(container, {}),
    };
  });
  assert.equal(result.selector.quote.exact, "Read this passage carefully.");
  assert.equal(
    result.selector.position.end - result.selector.position.start,
    result.selector.quote.exact.length,
  );
  assert.equal(result.reanchored, result.selector.quote.exact);
  assert.equal(result.emptyQuote, null);
  assert.equal(result.blankQuote, null);
  assert.equal(result.missingQuote, null);
});
