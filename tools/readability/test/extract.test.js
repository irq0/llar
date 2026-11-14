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
  assert.doesNotMatch(
    result.sourceSnapshot,
    /<script|<iframe|<form|<input|<button|<meta|\sstyle=|\sautoplay/i,
  );
  assert.match(result.sourceSnapshot, /data-src="\/images\/article.jpg"/);
  assert.match(
    result.content,
    /src="https:\/\/example\.com\/images\/article\.jpg"/,
  );
});

test("extracts a configured email body without losing headings, linked images, or data tables", async () => {
  const paragraph =
    "A newsletter paragraph about building reliable software systems. ".repeat(
      20,
    );
  const html = `<html><body><p>Subscribe and unsubscribe controls</p>
    <div class="body markup"><div style="display:none">Hidden inbox preview</div>
      <h3 class="header-anchor-post">Architecture</h3><p>${paragraph}</p>
      <div class="captioned-image-container"><div><table><tbody><tr><td></td><td>
        <a href="https://example.com/image"><img src="/blob/abc123" alt="Architecture diagram"></a>
      </td><td></td></tr></tbody></table></div></div>
      <h3 class="header-anchor-post">Results</h3><p>${paragraph}</p>
      <table><caption>Measurements</caption><tr><th>System</th><th>Latency</th></tr>
        <tr><td>Primary</td><td>25 ms</td></tr></table>
      <script>alert('unsafe')</script><p style="color:red">${paragraph}</p>
    </div><footer>Unsubscribe</footer></body></html>`;
  const result = await extract(html, "https://example.com/newsletter", {
    from: "contents",
    selector: ".body.markup",
  });
  assert.match(result.content, /<h3>Architecture<\/h3>/);
  assert.match(result.content, /<h3>Results<\/h3>/);
  assert.match(result.content, /src="\/blob\/abc123"/);
  assert.match(result.content, /href="https:\/\/example.com\/image"/);
  assert.match(result.content, /<caption>Measurements<\/caption>/);
  assert.doesNotMatch(
    result.content,
    /Subscribe|Unsubscribe|Hidden inbox preview|<script|style=/,
  );
  assert.equal((result.content.match(/<table/g) || []).length, 1);
  assert.match(result.sourceSnapshot, /class="body markup"/);
  assert.match(result.sourceSnapshot, /Unsubscribe/);
  assert.match(result.sourceSnapshot, /hidden=""/);
  const repeated = await extract(
    result.sourceSnapshot,
    "https://example.com/newsletter",
    {
      from: "contents",
      selector: ".body.markup",
    },
  );
  assert.match(repeated.content, /<h3>Architecture<\/h3>/);
  assert.doesNotMatch(repeated.content, /Hidden inbox preview/);
  assert.deepEqual(
    await extract(html, "https://example.com/", { selector: ".missing" }),
    {},
  );
  const withoutUrl = await extract(html, null, { selector: ".body.markup" });
  assert.match(withoutUrl.content, /src="\/blob\/abc123"/);
  assert.match(withoutUrl.content, /<h3>Architecture<\/h3>/);
});

test("website blob paths still resolve to the publisher", async () => {
  const result = await extract(
    articleBody.replace("/images/article.jpg", "/blob/article.jpg"),
    "https://example.com/article",
  );
  assert.match(
    result.content,
    /src="https:\/\/example\.com\/blob\/article\.jpg"/,
  );
  const direct = await extract(
    articleBody
      .replace('data-src="/images/article.jpg"', 'src="/blob/direct.jpg"')
      .replace('src="data:image/gif;base64,placeholder"', ""),
    "https://example.com/article",
  );
  assert.match(
    direct.content,
    /src="https:\/\/example\.com\/blob\/direct\.jpg"/,
  );
});

test("snapshot remembers the document base URL for later extraction", async () => {
  const result = await extract(
    `<html><head><base href="https://cdn.example.com/posts/"></head><body>${articleBody}</body></html>`,
    "https://example.com/article",
  );
  assert.equal(result.sourceSnapshotUrl, "https://cdn.example.com/posts/");
  const repeated = await extract(
    result.sourceSnapshot,
    result.sourceSnapshotUrl,
    { from: "contents" },
  );
  assert.match(
    repeated.content,
    /src="https:\/\/cdn\.example\.com\/images\/article\.jpg"/,
  );
});

test("snapshot replay preserves title, document attributes, and selector context", async () => {
  const html = `<html lang="en"><head><title>Visible article title</title></head>
    <body class="article-page" data-section="engineering">${articleBody}</body></html>`;
  for (const selector of [
    undefined,
    "body.article-page",
    'html[lang="en"] body[data-section="engineering"] article',
  ]) {
    const first = await extract(html, "https://example.com/article", {
      selector,
    });
    assert.match(first.sourceSnapshot, /<title>Visible article title<\/title>/);
    assert.match(first.sourceSnapshot, /<html lang="en">/);
    assert.match(
      first.sourceSnapshot,
      /<body class="article-page" data-section="engineering">/,
    );
    const replay = await extract(
      first.sourceSnapshot,
      first.sourceSnapshotUrl,
      { from: "contents", selector },
    );
    assert.equal(replay.title, first.title);
    assert.equal(replay.textContent, first.textContent);
  }
});

test("parses the full article without executing inline or blocking external scripts", async () => {
  const html = `<!doctype html><html><head>
    <title>Passive article</title>
    <script src="https://attacker.invalid/blocking.js"></script>
  </head><body>
    <script>document.documentElement.textContent = "Page script executed";</script>
    ${articleBody}
    <p>End of the complete article.</p>
  </body></html>`;

  const result = await extract(html, "https://example.com/article");

  assert.match(result.content, /Visible article title/);
  assert.match(result.content, /End of the complete article/);
  assert.doesNotMatch(result.content, /Page script executed|<script/i);
});
