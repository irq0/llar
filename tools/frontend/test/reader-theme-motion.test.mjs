import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";

import { chromium } from "playwright";

const repositoryRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../..",
);
const asset = (...parts) =>
  path.join(repositoryRoot, "resources/status", ...parts);
const browserChannel = process.env.LLAR_BROWSER_CHANNEL ?? "chrome";

async function renderThemeFixture(page, theme) {
  await page.setContent(`
    <!doctype html>
    <html>
      <body class="llar-reader"${theme ? ` data-bs-theme="${theme}"` : ""}>
        <main>
          <span class="reader-theme-faint">Tertiary metadata</span>
          <div class="reader-tool-message reader-tool-message-error">An error</div>
          <div class="reader-related-signals">
            <span class="reader-related-signal reader-related-signal-relative">80% of top</span>
            <span class="reader-related-signal reader-related-signal-rank">search 0.125</span>
            <span class="reader-related-signal reader-related-signal-title">title 0.050</span>
          </div>
          <select class="form-select reader-search-filter-select"><option>Any time</option></select>
          <div class="dropdown-menu show"><a class="dropdown-item" href="#">Item</a></div>
          <div class="modal-content"><button class="btn-close" aria-label="Close"></button></div>
          <span class="reader-lifecycle-dot"></span>
          <span class="reading-step-indicator"></span>
          <button class="reading-checkpoint-control"></button>
          <div class="checkpoint-resume-target"></div>
        </main>
      </body>
    </html>
  `);
  await page.addStyleTag({
    path: asset("bootstrap", "css", "bootstrap.min.css"),
  });
  await page.addStyleTag({ path: asset("llar.css") });
}

test("Automatic dark mode keeps Bootstrap and Reader surfaces coherent", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage();
  await page.emulateMedia({ colorScheme: "dark" });
  await renderThemeFixture(page);

  const theme = await page.evaluate(() => {
    const style = (selector) =>
      getComputedStyle(document.querySelector(selector));
    const body = style("body");
    const error = style(".reader-tool-message-error");
    return {
      surface: body.getPropertyValue("--llar-surface").trim(),
      faint: body.getPropertyValue("--llar-ink-faint").trim(),
      errorColor: error.color,
      errorBackground: error.backgroundColor,
      relativeColor: style(".reader-related-signal-relative").color,
      relativeBackground: style(".reader-related-signal-relative")
        .backgroundColor,
      rankBackground: style(".reader-related-signal-rank").backgroundColor,
      titleColor: style(".reader-related-signal-title").color,
      titleBackground: style(".reader-related-signal-title").backgroundColor,
      selectBackground: style(".form-select").backgroundColor,
      dropdownBackground: style(".dropdown-menu").backgroundColor,
      modalBackground: style(".modal-content").backgroundColor,
      closeFilter: style(".btn-close").filter,
    };
  });

  assert.equal(theme.surface, "#202220");
  assert.ok(contrast(theme.faint, theme.surface) >= 4.5);
  assert.ok(contrast(theme.errorColor, theme.errorBackground) >= 4.5);
  assert.equal(theme.errorBackground, "rgb(44, 11, 14)");
  assert.equal(theme.relativeColor, "rgb(33, 37, 41)");
  assert.equal(theme.relativeBackground, "rgb(255, 154, 87)");
  assert.equal(theme.rankBackground, "rgb(41, 43, 41)");
  assert.ok(contrast(theme.titleColor, theme.titleBackground) >= 4.5);
  assert.equal(theme.titleColor, "rgb(255, 178, 123)");
  assert.equal(theme.titleBackground, "rgb(75, 44, 28)");
  assert.equal(theme.selectBackground, "rgb(48, 50, 48)");
  assert.equal(theme.dropdownBackground, "rgb(32, 34, 32)");
  assert.equal(theme.modalBackground, "rgb(48, 50, 48)");
  assert.notEqual(theme.closeFilter, "none");
  await page.close();
});

test("Explicit light mode remains light under a dark system preference", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage();
  await page.emulateMedia({ colorScheme: "dark" });
  await renderThemeFixture(page, "light");

  const theme = await page.evaluate(() => {
    const body = getComputedStyle(document.body);
    const title = getComputedStyle(
      document.querySelector(".reader-related-signal-title"),
    );
    return {
      surface: body.getPropertyValue("--llar-surface").trim(),
      faint: body.getPropertyValue("--llar-ink-faint").trim(),
      titleColor: title.color,
      titleBackground: title.backgroundColor,
      closeFilter: getComputedStyle(document.querySelector(".btn-close"))
        .filter,
    };
  });

  assert.equal(theme.surface, "#fff");
  assert.ok(contrast(theme.faint, theme.surface) >= 4.5);
  assert.ok(contrast(theme.titleColor, theme.titleBackground) >= 4.5);
  assert.equal(theme.titleColor, "rgb(109, 47, 7)");
  assert.equal(theme.titleBackground, "rgb(254, 234, 220)");
  assert.equal(theme.closeFilter, "none");
  await page.close();
});

test("Reduced motion disables Reader scrolling and presentation transitions", async (t) => {
  const browser = await chromium.launch({
    channel: browserChannel,
    headless: true,
  });
  t.after(() => browser.close());

  const page = await browser.newPage();
  await page.emulateMedia({ reducedMotion: "reduce" });
  await renderThemeFixture(page);
  await page.addScriptTag({ path: asset("jquery", "jquery.min.js") });
  await page.evaluate(() => {
    window.jQuery.fn.popover = function () {
      return this;
    };
  });
  await page.addScriptTag({ path: asset("llar.js") });

  const motion = await page.evaluate(() => ({
    scroll: readingScrollBehavior(),
    dots: getComputedStyle(document.querySelector(".reader-lifecycle-dot"))
      .animationName,
    step: getComputedStyle(document.querySelector(".reading-step-indicator"))
      .transitionDuration,
    checkpoint: getComputedStyle(
      document.querySelector(".reading-checkpoint-control"),
    ).transitionDuration,
    flash: getComputedStyle(document.querySelector(".checkpoint-resume-target"))
      .transitionDuration,
  }));

  assert.equal(motion.scroll, "auto");
  assert.equal(motion.dots, "none");
  assert.equal(motion.step, "0s");
  assert.equal(motion.checkpoint, "0s");
  assert.equal(motion.flash, "0s");
  await page.close();
});

function contrast(foreground, background) {
  const values = [foreground, background].map(rgbChannels).map(luminance);
  return (Math.max(...values) + 0.05) / (Math.min(...values) + 0.05);
}

function rgbChannels(color) {
  if (color.startsWith("#")) {
    const hex = color.slice(1);
    const normalized =
      hex.length === 3 ? [...hex].map((x) => x + x).join("") : hex;
    return normalized.match(/../g).map((value) => Number.parseInt(value, 16));
  }
  return color
    .match(/[\d.]+/g)
    .slice(0, 3)
    .map(Number);
}

function luminance(channels) {
  const [red, green, blue] = channels.map((value) => {
    const channel = value / 255;
    return channel <= 0.04045
      ? channel / 12.92
      : ((channel + 0.055) / 1.055) ** 2.4;
  });
  return 0.2126 * red + 0.7152 * green + 0.0722 * blue;
}
