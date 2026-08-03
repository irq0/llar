# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Observability: Export each recurring schedule's next run and the expected
  interval to the following run for cadence-aware Prometheus alerting.
- Observability: Export standard `hikaricp_*` metrics for the frontend and
  backend PostgreSQL connection pools.
- Observability: Export live capacity, utilization, queue depth, completion,
  and permit-wait metrics for source-update and item-processing executors plus
  command, media-download, and streaming throttles. Export aggregate in-flight
  work counts by kind/stage and the age of the oldest source and item work.
- Dashboard: New Activity tab replaces the raw Threads tab with bounded-resource
  saturation, queued work, mean waits, current source/item pipeline activity,
  stragglers, and a grouped thread census; full stack traces remain available in
  a collapsed diagnostic section.
- Schedules: New canned schedules `:noon`, `:now-and-noon`, `:every-4-hours`,
  `:now-and-every-4-hours`, and `:now-and-every-15-minutes`.
- Feeds: Support feeds whose entries have no title (notably Bluesky). The item
  title falls back to the entry description, GUID, or link, and the item hash
  falls back to the entry GUID.
- Streaming: `src/streaming-channel` now accepts playlist URLs in addition to
  channel URLs.
- Podcast: Dashboard "Untag" action removes the `:podcast` tag from an episode
  while keeping the downloaded blob, and permanently failed downloads are
  untagged when their tracking entry is cleaned up.
- HTTP: New `:http` config keys `:connection-timeout-ms` (default 10000),
  `:connection-request-timeout-ms` (default 10000), and `:socket-timeout-ms`
  (default 120000).
- Configuration: Add `:throttle` concurrency limits for source updates (default
  4) and item post-processing (default `:auto`, based on available processors),
  alongside the existing command, downloader, and streaming limits.

### Changed

- Readability: Extract passive Open Graph, Twitter Card, canonical, microdata,
  and JSON-LD metadata from the original inert document while keeping rendered
  article HTML on a separate hardened DOMPurify path.
- Media extraction: Remove global `metascraper-media-provider`, iframe/oEmbed,
  and service network lookups. Readability fetches no longer launch the bundled
  `yt-dlp`; explicitly podcast-tagged URLs use the existing configured system
  `yt-dlp` download and retry path.
- HTTP: Apply connection, connection-request, and socket timeouts to all
  outbound requests.
- Docker: Compose now runs `postgres:18` and mounts the database volume at
  `/var/lib/postgresql` instead of `/var/lib/postgresql/data`. See the upgrade
  notes below.
- Reader: YouTube previews now embed via `youtube-nocookie.com` and send
  `referrerpolicy=strict-origin-when-cross-origin`.
- Build: Move `clj-memory-meter` and its `-Djdk.attach.allowAttachSelf=true` JVM
  option to the `:dev` profile.
- Updates: Replace nested `pmap` calls with separate, named, bounded executors
  for source updates and item post-processing. Bulk updates now preserve input
  order, isolate per-source failures as structured outcomes, and apply the same
  global source limit to individual dashboard-triggered updates.
- Updates: Enforce atomic single-flight admission per source, including forced
  updates, so overlapping schedules, Reader requests, and Dashboard requests
  cannot fetch and store the same source concurrently.
- Throttling: Replace command, media-download, and streaming semaphores with
  managed, observable throttles using the configured capacities.
- Reddit: Migrate fetching from the retired unauthenticated `.json` endpoints to
  the OAuth2 Data API, with cached client-credentials tokens, synchronized
  renewal, one retry after a rejected token, and rate-limit logging. Dynamic
  score filtering now reuses the fetched listing instead of making a second
  request.
- Dependencies: Upgrade selected Clojure libraries (jsoup, Bouncy Castle, Apache
  Tika, OpenNLP, NewPipeExtractor, Migratus, and REPL tooling), Node.js metadata
  extraction dependencies (jsdom, DOMPurify, metascraper), and the vendored web
  assets Font Awesome and DataTables. DataTables moves from `3.0.0-beta.2` to the
  stable `3.0.1`.

### Fixed

- Commands: Fall back to a concurrency limit of 2 when `:throttle
  :command-max-concurrent` or `:throttle :av-downloader-max-concurrent` is absent
  from the configuration, instead of failing to construct the semaphore.
- Blobstore: Tolerate a broken blob properties file whose read error carries no
  content payload, which previously threw while recreating the file.
- Reader: Fix the lazy YouTube view so the preview thumbnail and the embedded
  player fill their aspect-ratio container.
- Updates: Prevent forced or racing requests from starting duplicate updates for
  one source, and prevent one unexpected source failure from aborting the rest
  of a bulk update result.
- Reddit: Drop malformed API entries instead of passing `nil` into
  post-processing, and correctly combine the configured minimum score with the
  dynamic top-5-percent cutoff.

### Upgrade notes

- **Docker: PostgreSQL 16 to 18 requires `pg_upgrade`.** From 18 on,
  the official image stores data in a major-version subdirectory
  (`PGDATA` is `/var/lib/postgresql/18/docker`), which is why the
  compose mount moved up to `/var/lib/postgresql`. An existing
  `llar-postgresql` volume from 4.0.0 holds a version 16 cluster at
  its root. The image detects this and refuses to start, exiting with
  an error that points at `pg_upgrade` — it does not silently create
  an empty cluster, so no data is destroyed, but `llar` will not come
  up until the cluster is migrated. Either run `pg_upgrade --link`
  with both major versions available, or dump and restore:
  `pg_dumpall` from a `postgres:16` container with the old volume
  mounted at `/var/lib/postgresql/data`, then restore into a fresh
  volume under the new layout.
- **Readability-sourced items re-import once.** Sanitization moved from the
  whole fetched document to the extracted article content under a stricter
  DOMPurify configuration, which changes the content that item hashes are
  derived from. Existing Readability items will be fetched again as new items on
  the next update.
- **Some feed entries re-import once.** Entries that have a title but no `link`
  now hash on their GUID rather than their title, so they will be fetched again
  as new items on the next update.
- **Reddit sources now require OAuth2 application credentials.** Create a
  Reddit "script" application and configure `:reddit {:app-id "..." :secret
  "..." :username "..."}` in `credentials.edn`; the old Reddit password is no
  longer used. The `:random` listing is no longer accepted because Reddit returns
  a single post rather than a listing, and `:best` remains a global front-page
  listing that ignores the configured subreddit.

## [4.0.0] - 2026-07-11

### Added

- Fever: Fever-compatible API for syncing LLAR with mobile feed readers,
  configured under `:api :fever` with dedicated credentials, port, and
  source-selection tag.
- Fever: Stable feeds and a flat LLAR group for selected sources, with
  incremental and ID-based item retrieval, bounded working sets, unread
  and saved state synchronization, and idempotent read/save actions.
- Fever: Serve content-addressed blobs through the Fever endpoint and
  rewrite stored blob references using its configured public base URL.
- Fever: Reader Reading Queue integration makes saved and in-progress
  items plus unread bookmarks available across all sources. Items from
  sources not otherwise exposed through Fever use a stable synthetic
  Reading Queue feed.
- Dashboard: Documentation tab with generated `.llar` config and
  system configuration reference.
- Configuration docs: Generate the appconfig reference from Clojure specs and
  effective defaults, document configuration sources and API services, and add
  PostgreSQL/HikariCP guidance shared by the dashboard and published docs.
- Reader: Reading Queue tool replaces the old Saved Overview label and
  shows saved items, in-progress items, and unread bookmarks with
  queue-reason badges, queue stats, and queue filters.
- Reader search: user-facing search modes for web-style queries, all
  words, phrase search, and legacy PostgreSQL `tsquery`.
- Reader search: result snippets with highlighted matches, source
  facets, source reset link, timestamps, and inline errors for invalid
  advanced queries.
- Search index migration: `search_index` now stores per-item search
  language, snippet text, and indexed body/description text alongside
  the existing title, author, URL, and NLP-derived terms.

### Changed

- Schedule subsystem: Dashboard can trigger, show last, next runs and
  results.
- Config Engine: Make declarative and integrate documentation.
- Config Engine: Support more runtime-changeable settings.
- Autoread status: include the printable predicate form in scheduler
  results while keeping the executable predicate function out of the
  status map.
- Reader search: default search now uses PostgreSQL
  `websearch_to_tsquery`; raw PostgreSQL query syntax remains available
  through the advanced mode.
- Reader search: ranking now uses cover-density ranking and search
  rows use their indexed language configuration for matching and
  snippets.
- Reader: Add reading time groups to Reading Queue
- Dependencies: Upgrade Clojure, selected Clojure libraries, Node.js
  metadata extraction dependencies, Bootstrap, Font Awesome, IBM Plex,
  and jQuery.
- Reader: Replace the Waypoints-based mark-read-on-view trigger with a
  native IntersectionObserver implementation, keeping the same
  bottom-in-view behavior while removing the Waypoints frontend
  dependency.
- Reader: Blobify preview media during postprocessing so persisted items
  are self-contained before they reach the UI.
- Reader: Show an explicit not-yet-compiled state for the Reading Queue,
  distinct from a successfully compiled empty queue.
- Blob delivery: Share Reader and Fever responses with immutable caching,
  conditional `304 Not Modified` support, validators, content length, and
  MIME-sniffing protection.
- Observability: Bound HTTP metric cardinality with abstract Reader route
  labels, separate item downloads and dumps, and classify Fever traffic by
  semantic calls instead of URL paths.
- Configuration: Enable API services by the presence of their `:port` and
  remove the unused `:enabled` flag from the dashboard and reader config.
- Docker: Shrink the runtime image by installing build tooling only
  transiently and purging it after builds, cleaning apt lists, and adding a
  `.dockerignore`.

### Fixed

- Reader: Fix mark-read-on-view timing and the explicit mark-items-read
  action.
- Podcast: Treat permanent downloader failures as final instead of
  retrying them indefinitely.
- Database: Normalize zoned timestamps to UTC before persistence.

### Security

- Reader: Strip common tracking parameters from item and content URLs,
  apply no-referrer link and response policies, and add a Content Security
  Policy.
- HTTP: Add configurable response-body and blob download size limits,
  enforced while streaming so oversized or decompression-bomb responses are
  rejected without buffering the whole body.
- Fever: Redact the API key and other credentials from request and
  response logs.

## [3.1.0] - 2026-06-21

### Changed

- Silence log4j2 INFO messages on startup
- Prometheus metrics overhaul: Add fine-grained metrics to track the
  source update process. Track postprocessing exceptions
- Dashboard: Load tabs on demand
- Dashboard: Add overview tab

### Fixes

- Repl: Fix repl startup. Broke due to dependency upgrade.
- Prometheus: Set Content-type version

## [3.0.0] - 2026-06-20

### Added

#### Podcast System

LLAR can now turn audio or video sources into a private podcast feed.
It downloads the media and serves a per-source and combined RSS feed.
The feed URL must be public to work with most podcast clients. Tested
with Podcasts on the Apple TV.

Because downloaded media adds up, each source keeps only a
configurable number of recent episodes and automatically deletes older
ones. The dashboard shows how much disk the whole library is using.

- Media download via yt-dlp with automatic subtitle extraction and chapter support
- Count-based episode retention with per-source limits
- Podcast disk usage stats on dashboard

#### Digest Magazines (e-reader)

Instead of reading everything in the browser, you can now mark items
with a :digest tag and have LLAR bundle them into an EPUB magazine
and email them to your e-reader.

- Collect items tagged `:digest` into an EPUB "magazine" and email it
  to an e-reader (tested with Kindle) on a schedule
- Tag-based state machine: `:digest` queues an item, `:digest-issue-N`
  records the issue it was sent in
- Issue-windowed autoread: keep the most recent `:keep-unread-issues`
  issues unread, auto-clear `:unread` on older ones
- Reader shows a `:digest` tag button to queue items, but only when
  the feature is configured
- Each article shows author, source, absolute date and a reading-time
  estimate, with links to the original and back into LLAR (`:api
  :reader :base-url`)
- Cover/TOC page (title, date, article count, grouped contents) and
  chapters grouped by source
- Configured under `:api :digest`; sends via the general mail transport

#### Streaming Channel Source

A new source type built on NewPipeExtractor lets you follow video channels.

#### Ranked Sorting

The reader list no longer has to be strictly newest-first. A new
ranked sort boosts items you've highlighted and items from sources
that post rarely. The boost weights are configurable, and a new
dashboard analytics tab lets you see how the ranking is behaving.

- Reader list sorting by highlight boost and source rarity
- Configurable boost weights via appconfig
- Dashboard ranking analytics tab

#### Annotations

You can highlight and annotate items right in the reader frontend.
Annotations can be exported with the item to Zotero through the Zotero
API, or hand it off via a URL handler such as org-protocol into
org-roam.

#### Export

- Export item + annotations via url handler (e.g org-protocol)
- Export item + annotations to Zotero via Zotero API

#### GitHub Fetcher

You can now add a GitHub repository as a source and follow its activity.

#### Outgoing Email

- Generic outgoing mail transport, used by the digest feature and
  reusable by other features

### Changed

- Dashboard: fetch stats display (fetched/processed/db)
- Reader: podcast tag button, HTML entity replacement with Unicode
- Tags: normalization to lower case
- Postproc: preserve error context in processing pipeline
- Update: classify Rome parse failures as permanent failures
- Docker: expose podcast port 8024, add fonts-noto-color-emoji
- Node.js tooling: removed unused browserify/polyfill packages, cutting
  the dependency tree roughly in half and resolving all known npm
  advisories; upgraded jsdom (26 → 29), metascraper (5.51) and DOMPurify

### Fixes

- Reader: fix HTML escaping
- HTTP: fix retry-later error message, fix version in user-agent
- DB: handle ByteBuffer in prep statements, fix migration version collision
- Dashboard: access frontend-db as value not function
- Podcast: fix propsfile reading, byte-range fix, source-key type mismatch in per-source feeds
- Kaocha: junit test file fix
- Human: force US locale for duration formatting

## [2.0.0] - 2026-02-22

### Added

- Subtitle downloader for video content sources
- Video content display shows both video player and description together.
- Reader focus mode for the headlines list view.
- "Show in focus mode" on the main content list.
- Raw content button for email entries.
- Word-length based reading difficulty heuristic (easy / medium / hard labels).
- Compact headline view redesign.
- IMAP nested multipart message handling.
- Node.js tooling: Mozilla Readability and DOMPurify wrappers (`tools/readability/`,
  `tools/dompurify/`) bundled and built as part of the project.
- Kaocha test runner with coverage (Cloverage) and JUnit XML output.
- Testcontainers-based integration tests against a real PostgreSQL instance.

### Changed

- `sources.data` column type changed from `hstore` to `jsonb`. Migration is automatic on the next start.
- Article extraction engine replaced. Postlight Mercury removed; Mozilla Readability (via
  Node.js) is now used.
- Live feed feature removed: `src/llar/live.clj` and related code deleted (was abandoned anyway).
- Raw bookmark feature removed: bookmark raw-content capture and HTML summary generation removed.
- Hiccup upgraded 1.x → 2.x
- Database layer migrated HugSQL → next.jdbc
- Docker base image updated to java 25; LTS Node.js from NodeSource; yt-dlp;
- Consolidated all GitHub Actions into a single "ci.yml"
- Use newer OpenNLP UD models from Maven JARs

### Fixes

- NLP data removed from item entry JSON. It should not have been there in the first place.
  Database migrations take care of stripping the fields.
- IMAP entry/raw content mixup.
- SAX parser compatibility fix for Java 25
- NLP: Fix broken english noun extraction

## [1.1.0] - 2024-12

### changes not individually documented — see git log

## [1.0.0] - 2024

### initial tagged release

[Unreleased]: https://github.com/irq0/llar/compare/4.0.0...HEAD
[4.0.0]: https://github.com/irq0/llar/compare/3.1.0...4.0.0
[3.1.0]: https://github.com/irq0/llar/compare/3.0.0...3.1.0
[3.0.0]: https://github.com/irq0/llar/compare/2.0.0...3.0.0
[2.0.0]: https://github.com/irq0/llar/compare/1.1.0...2.0.0
[1.1.0]: https://github.com/irq0/llar/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/irq0/llar/releases/tag/1.0.0
