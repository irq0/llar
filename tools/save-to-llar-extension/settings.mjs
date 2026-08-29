const tokenPattern = /^[A-Za-z0-9_-]{32,}$/;

export function normalizeCaptureBase(value) {
  const url = new URL(value);
  if (url.protocol !== "https:" && url.protocol !== "http:") {
    throw new Error("Capture URL must use HTTPS or HTTP.");
  }
  if (url.username || url.password || url.search || url.hash) {
    throw new Error(
      "Capture URL must not contain credentials, a query, or a fragment.",
    );
  }
  url.pathname = url.pathname.replace(/\/+$/, "") + "/";
  return url.toString();
}

export function validCaptureToken(value) {
  return tokenPattern.test(value);
}

export function makeFeedbackUrl(captureBase, token, pageUrl) {
  if (!validCaptureToken(token)) {
    throw new Error("Capture token is missing or invalid.");
  }
  if (!/^https?:\/\//i.test(pageUrl)) {
    throw new Error("The current page is not an HTTP or HTTPS page.");
  }
  return normalizeCaptureBase(captureBase) + "#" + token + ":" + pageUrl;
}
