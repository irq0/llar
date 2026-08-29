import assert from "node:assert/strict";
import test from "node:test";

import {
  makeFeedbackUrl,
  normalizeCaptureBase,
  validCaptureToken,
} from "../settings.mjs";

const token =
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

test("capture base URLs are normalized without losing a proxy path", () => {
  assert.equal(
    normalizeCaptureBase("https://save.example.org/llar"),
    "https://save.example.org/llar/",
  );
  assert.equal(
    normalizeCaptureBase("http://localhost:8026/"),
    "http://localhost:8026/",
  );
});

test("capture base URLs reject ambiguous or unsafe components", () => {
  assert.throws(
    () => normalizeCaptureBase("file:///tmp/capture"),
    /HTTPS or HTTP/,
  );
  assert.throws(
    () => normalizeCaptureBase("https://save.example.org/?token=secret"),
    /must not contain/,
  );
});

test("capture tokens use the same bounded alphabet expected by LLAR", () => {
  assert.equal(validCaptureToken(token), true);
  assert.equal(validCaptureToken("too-short"), false);
  assert.equal(validCaptureToken("x".repeat(31) + ":"), false);
});

test("feedback URL preserves the captured page URL", () => {
  assert.equal(
    makeFeedbackUrl(
      "https://save.example.org/llar",
      token,
      "https://example.org/article?id=3#notes",
    ),
    "https://save.example.org/llar/#" +
      token +
      ":https://example.org/article?id=3#notes",
  );
});
