# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
