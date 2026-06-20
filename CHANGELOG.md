# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

#### Podcast System

- Media download via yt-dlp with automatic subtitle extraction and chapter support
- Count-based episode retention with per-source limits
- Podcast disk usage stats on dashboard

#### Digest Magazines (e-reader)

- Collect items tagged `:digest` into an EPUB "magazine" and email it
  to an e-reader (Kindle, PocketBook, Onyx Boox, ...) on a schedule
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
- Configured under `:api :digest`; sends via the general `:mail` transport

#### Streaming Channel Source

- New StreamingChannel source type using NewPipeExtractor

#### Ranked Sorting

- Reader list sorting by highlight boost and source rarity
- Configurable boost weights via appconfig
- Dashboard ranking analytics tab

#### Annotations

- Annotatate items in the reader frontend

#### Export

- Export item + annotations via url handler (e.g org-protocol)
- Export item + annotations to Zotero via Zotero API

#### GitHub Fetcher

- New GitHub source type for fetching repository activity

### Changed

- Dashboard: fetch stats display (fetched/processed/db)
- Reader: podcast tag button, HTML entity replacement with Unicode
- Tags: normalization to lower case
- Postproc: preserve error context in processing pipeline
- Update: classify Rome parse failures as permanent failures
- Docker: expose podcast port 8024, add fonts-noto-color-emoji

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

[Unreleased]: https://github.com/irq0/llar/compare/2.0.0...HEAD
[2.0.0]: https://github.com/irq0/llar/compare/1.1.0...2.0.0
[1.1.0]: https://github.com/irq0/llar/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/irq0/llar/releases/tag/1.0.0
