const assert = require("node:assert/strict");
const test = require("node:test");

const { extract } = require("../index");

const articleBody = `
  <article>
    <h1>Visible article title</h1>
    <p>${"A sufficiently long article sentence. ".repeat(20)}</p>
    <img src="data:image/gif;base64,placeholder" data-src="/images/article.jpg"
         data-tracking-id="secret" style="position: fixed">
  </article>`;

test("extracts passive metadata from the original head", async () => {
  const html = `<!doctype html><html><head>
    <title>HTML title</title>
    <link rel="canonical" href="https://example.com/canonical">
    <meta property="og:title" content="Open Graph title">
    <meta property="og:description" content="Metadata description">
    <meta property="og:image" content="https://cdn.example.com/cover.jpg">
    <meta property="article:published_time" content="2026-07-18T10:00:00.000Z">
    <meta name="author" content="Ada Author">
    <script type="application/ld+json">{
      "@context":"https://schema.org", "@type":"NewsArticle",
      "headline":"JSON-LD title", "publisher":{"name":"Example Publisher"}
    }</script>
  </head><body>${articleBody}</body></html>`;

  const result = await extract(html, "https://example.com/source");

  assert.equal(result.metadata.title, "Open Graph title");
  assert.equal(result.metadata.description, "Metadata description");
  assert.equal(result.metadata.image, "https://cdn.example.com/cover.jpg");
  assert.equal(result.metadata.author, "Ada Author");
  assert.equal(result.metadata.url, "https://example.com/canonical");
  assert.equal(result.metadata.publisher, "Example Publisher");
});

test("sanitizes rendered content while preserving normalized lazy images", async () => {
  const html = `<!doctype html><html><head>
    <meta http-equiv="refresh" content="0; url=https://attacker.invalid">
    <script>globalThis.executed = true</script>
  </head><body>
    ${articleBody}
    <form action="https://attacker.invalid"><input name="secret"><button>Send</button></form>
    <iframe src="https://attacker.invalid/embed"></iframe>
    <video autoplay style="position: fixed"><source src="/movie.mp4"></video>
  </body></html>`;

  const result = await extract(html, "https://example.com/article");

  assert.doesNotMatch(result.content, /<script|<iframe|<form|<input|<button/i);
  assert.doesNotMatch(result.content, /\sstyle=|\sautoplay|data-tracking-id/i);
  assert.match(
    result.content,
    /src="https:\/\/example\.com\/images\/article\.jpg"/,
  );
});
