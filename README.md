# 🖖

Live Long and Read!

LLAR is a self-hosted reading system with programmable inputs and curation.

You define what to fetch, when to fetch it, and what should happen to the
result. Feeds, websites, newsletters, social timelines, videos, and bookmarks
are normalized into one item model, then filtered, transformed, tagged, ranked,
and presented by the Reader.

The configuration and processing rules are yours. The resulting items are
stored in PostgreSQL and remain available to search and revisit.

> This README and its screenshots describe the current `main` branch and may
> include features not yet available in the [latest release](https://github.com/irq0/llar/releases/latest).

![The LLAR Reader populated with deterministic demo content](https://irq0.github.io/llar/demo/main/reader.png)

## Why LLAR?

- **Many inputs, one model.** RSS, HTML, APIs, mail, media, and saved URLs can
  share the same item fields, tags, search, and reading state.
- **Curation in configuration.** `.llar` files define sources, schedules,
  source groups, filters, processors, highlights, and Reader defaults.
- **Small programmable steps.** Clojure functions can remove noise, select
  items, add metadata, rewrite content, or route items by tag.
- **Different views for different jobs.** Preview, Headlines, Gallery,
  Reading Queue, Gems, Search, and Today’s Vibe are derived from the same
  items, with the underlying data and rules left inspectable.

## Getting Started

LLAR needs PostgreSQL, a JVM, and a few command-line tools listed in
[resources/config.edn](resources/config.edn). The easiest way to start is with
Docker Compose and the [LLAR container image](https://github.com/irq0/llar/pkgs/container/llar).

```sh
git clone https://github.com/irq0/llar.git
cd llar/docker
docker compose pull
docker compose up
```

Open the Reader at <http://localhost:8023> and the Dashboard at
<http://localhost:9999>.

### Add your first sources

If you use Docker Compose as above, runtime configuration lives in `llar/config`.
LLAR watches that directory for change.

Here is a small example with xkcd, GitHub issues, and the Hacker News front
page. It demonstrates that LLAR can combine an independent RSS feed with
API-backed sources in the same Reader.

Put the following in `llar/config/myfirstconfig.llar`:

```clojure
(fetch xkcd (src/feed "https://xkcd.com/rss.xml")
  :tags #{:my-first-feed :comics})
(fetch llar-issues (src/github-issues "repo:irq0/llar is:issue is:open")
  :tags #{:my-first-feed :github})
(fetch hn-frontpage (src/hn :front_page) :tags #{:my-first-feed :hackernews})
(sched-fetch my-first-feeds :now-and-hourly (some #{:my-first-feed} $TAGS))
```

What does this do?

`fetch` defines a named source and how LLAR should fetch it. Here `feed` reads
an RSS URL, `github-issues` searches GitHub using GitHub's issue-search
syntax, and `hn` reads a Hacker News view.

The GitHub example watches LLAR's own open issues. Replace `irq0/llar` with a
repository you care about, for example `repo:clojure/clojure is:issue is:open`.

`xkcd`, `llar-issues`, and `hn-frontpage` are source keys:
stable, user-defined identifiers for the items produced by each source.

The `:tags` options group sources with source tags. Tags can later select
sources for schedules, views, mobile sync, or processing. The `#{...}` syntax
is a Clojure set of keywords.

The last line creates a fetch schedule. It updates every source tagged
`:my-first-feed` shortly after the configuration loads and then once an hour.
Schedules are part of the curation model: you decide which inputs are active
and how often they contribute new material.

### Have an OPML file?

Awesome! Copy it to the config directory and let LLAR convert it for you.
If you use docker please ensure that LLAR can write to the config directory.

The generated file will have the extension `.llar.example`.
I suggest that you have a look and adjust the generated source keys.
If you want LLAR to load the config, just rename it to `.llar` and it will load the file.

## What it can do

- Sources: RSS, Atom, WordPress REST, HTML selectors, Reddit, Hacker News,
  GitHub, IMAP, social timelines, websites, and streaming channels
- One normalized item model for feeds, articles, newsletters, bookmarks, and
  media
- Article extraction via Mozilla Readability with title, author, date,
  description, lead image, HTML, and plain text
- HTML sanitization, absolute URLs, local image blobs, and safe fallback to the
  original item when extraction fails
- Clojure-configurable sources and schedules, plus scriptable filters,
  processors, highlights, tags, and Reader defaults
- Durable PostgreSQL storage for ordinary fetched items, with no generic
  age-based cleanup
- Reader UI with Preview, Headlines, Gallery, Search, Reading Queue, Continue
  Reading, Gems, and Today’s Vibe
- Separate unread, saved, archived, and reading-checkpoint states with finite
  reading batches
- Bookmark and save-for-later capture through the Reader or an authenticated API
- Private podcast feeds built from audio and video sources, with
  downloads, subtitles, chapters, and configurable per-source media retention
- Scheduled EPUB digests that bundle tagged items into e-reader magazines
- Export items and annotations to Zotero or configured URL handlers
- Fever-compatible mobile sync, Dashboard diagnostics, and Config Lab

## UI

### Reader

By default, the Reader runs on port 8023.

The same demo deployment also shows the main Reader tools:

| Reading Queue | Continue Reading |
| --- | --- |
| ![LLAR Reading Queue](https://irq0.github.io/llar/demo/main/reading-queue.png) | ![LLAR Continue Reading](https://irq0.github.io/llar/demo/main/continue-reading.png) |

| Gems | Today’s Vibe |
| --- | --- |
| ![LLAR Gems](https://irq0.github.io/llar/demo/main/gems.png) | ![LLAR Today’s Vibe](https://irq0.github.io/llar/demo/main/todays-vibe.png) |

### Dashboard

By default, the Dashboard runs on port 9999.

![The LLAR Dashboard start page](https://irq0.github.io/llar/demo/main/dashboard.png)

Makes internal application state accessible.
This includes memory usage,
database stats,
schedules,
application state,
threads and
configuration.

Gives a *technical* view on the fetch status of configured sources.
Shows timing and error information and allows manual triggers.

### Mobile readers (Fever)

LLAR can expose selected sources and the Reader Reading Queue through a
Fever-compatible API. Configure a dedicated port and credential in the
system configuration:

```clojure
{:api {:fever {:port 8025
               :base-url "https://llar.example/fever"
               :username "llar"
               :credentials :mobile-sync
               :source-tag :mobile}}}
```

Add the password to the configured credentials file:

```clojure
{:mobile-sync {:password "use-a-dedicated-password"}}
```

Sources tagged `:mobile` are exposed as regular Fever feeds. Saved items and
active reading checkpoints from all sources are available in
the Reading Queue. Point the client at the Fever port through an HTTPS
reverse proxy and use the configured username and password.

### Save for later

LLAR can accept URLs independently of the Reader UI through a durable capture
queue. Configure a dedicated HTTPS service and a named credentials entry:

```clojure
;; system config, inside :api
:capture {:port 8026
          :base-url "https://save.example.org"
          :credentials :bookmark-capture
          :schedule :now-and-every-minute}

;; credentials.edn; generate each value with: openssl rand -hex 32
{:bookmark-capture
 {:tokens {:iphone "replace-with-64-hex-characters"
           :firefox "replace-with-64-hex-characters"}}}
```

The API is `POST /api/v1/captures` with JSON `{"url":"https://..."}` and
`Authorization: Bearer <token>`. Each token is named and independently
revocable. Captures receive immediate feedback only after their queue row is
committed; extraction happens asynchronously. Setup instructions for Firefox,
Chrome, iOS Shortcuts, macOS Shortcuts, queue recovery, and alerts are in the
Dashboard's Docs tab.

## Concept

LLAR is built around a small, durable item pipeline:

```text
sources → fetchers → normalized items → processors and filters → PostgreSQL → Reader
```

The [updater](src/llar/update.clj) [fetches](src/llar/fetch.clj)
[sources](src/llar/src.clj), normalizes their output into items, runs the items
through the [processor](src/llar/postproc.clj), and finally
[persists](src/llar/persistency.clj) them. The same item model powers live
feeds, extracted articles, captured bookmarks, search, mobile sync, and the
Reader UI.

`.llar` files specify sources, schedules, processing rules, highlights, and
Reader options. A fetch definition can contain pre-processing, post-processing,
filters, UI options, and source tags. LLAR watches the runtime configuration
directory, so many changes do not require a restart.

Each item has a title, timestamps, source identity, content, descriptions,
media and tags. Processors and filters are Clojure functions; within the item
spec they can reshape the material to suit your workflow. HTML is sanitized,
URLs are repaired, and readability extraction can turn a link or a feed
summary into a proper article.

There are two different kinds of tags. Source tags group inputs and are useful
for schedules and integrations. Item tags describe individual items. Workflow
states are separate: `unread`, `saved`, and `archive` are semantic actions,
not arbitrary labels. `Continue Reading` stores a reading checkpoint rather
than pretending progress is a tag.

That separation is one of LLAR’s core ideas: configuration describes how
material enters and is shaped; item metadata helps you find it; reading state
records what you intend to do with it.

### Retention

The item store is deliberately durable: ordinary fetched items are kept in
PostgreSQL and there is no generic “older than N days” cleanup. This makes old
material available to search and revisit without making the archive itself a
separate knowledge-management system.

Podcast media is the deliberate exception: downloaded episodes can use an
explicit count-based retention policy so large audio/video files do not grow
without bound. That policy concerns podcast retention and storage; it is not a
general expiry rule for the item archive.

## Configuration

### System config

Loaded on startup. Contains paths to command line tools, blob store,
API ports, database pools, credentials file location, and other host-level settings.

Examples:

- [config.edn](resources/config.edn)
- [docker-config.edn](docker/docker-config.edn)

### Config

Runtime configuration. Automatically loaded when files in the `:runtime-config-dir` change.
Specify sources to fetch, schedules, highlight rules here.

Files are (almost) Clojure code with extra constructs for convenience. They use the extension `.llar`.
Runtime behavior settings can be configured with `rc` and higher-level constructs.
See the [configuration documentation](https://irq0.github.io/llar/config.html)
or check out [my config](https://github.com/irq0/llar-config).

### Credentials

The credentials file contains secrets made available with the `$credentials` function in `.llar` config files.
See [credentials.edn.example](resources/credentials.edn.example).

### Config Lab

Config Lab is an opt-in Dashboard tab for test-driving source configuration
without loading a `.llar` file. Enable it inside the Dashboard configuration:

```clojure
:api {:dashboard
      {:port 9999
       :config-lab {:enabled? true
                    :credentials :config-lab
                    :max-concurrent-runs 2
                    :run-timeout-ms 20000
                    :session-ttl-minutes 30}}}
```

Generate a dedicated token and put it only in `credentials.edn`:

```sh
openssl rand -hex 32
```

```clojure
{:config-lab
 {:tokens {:dashboard "replace-with-the-generated-value"}}}
```
