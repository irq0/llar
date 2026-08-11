# 🖖

Live Long and Read!

A self-hosted news aggregator focused on customizability.

> This README and its screenshots describe the current `main` branch and may
> include features not yet available in the [latest release](https://github.com/irq0/llar/releases/latest).

## Getting Started

LLAR needs a PostgreSQL database, a JVM and a [couple of command line tools](resources/config.edn) to run.
Easiest way to get started is to use docker compose with the [llar container image](https://github.com/irq0/llar/pkgs/container/llar).

```sh
git clone https://github.com/irq0/llar.git
cd llar/docker
docker-compose pull
docker-compose up
```

### Add your first feed

If you use the docker compose way above, the configuration  lives in `llar/config`.
LLAR watches that directory for change.

Let's add an RSS feed and the Hacker News front page as an example.

this projects GitHub release feed as an example. Put the following in `llar/config/myfirstconfig.llar`.

```clojure
(fetch github-llar-releases (src/feed "https://github.com/irq0/llar/releases.atom") :tags #{:my-first-feed :github})
(fetch hn-frontpage (src/hn :front_page) :tags #{:my-first-feed :hackernews})
(sched-fetch my-first-feeds :now-and-hourly (some #{:my-first-feed} $TAGS))
```

What does this do?

`fetch` instructs LLAR how to fetch a *source*, it's name and various options.
In this example we have a `feed` source with URL `https://github.com/irq0/llar/releases.atom` and a `hn`
source which takes a Hacker News search tag instead of an address.
Feed sources are the most common and support RSS, Atom and the like.

`github-llar-releases` and `hn-frontpage` are *source key*. A user-defined identifier for all data fetched from this source.

`:tags #{:my-first-feed :github}` instructs LLAR to tag this source with `:my-first-feed` and `:github`.
This is Clojure syntax to say "set the fetch function's tags keyword argument to a set of two keywords".
Keywords in this case behave much like strings, except that you really shouldn't use whitespace in them.

The last line creates a *fetch schedule*. LLAR is quite flexible in when to update what.
In this case we define a schedule `my-first-feeds` that updates all sources tagged `:my-first-feed`
now (0-120 seconds after LLAR loads the schedule) and then every hour.

### Have an OPML file?

Awesome! Copy it to the config directory and let LLAR convert if for you.
If you use docker please ensure that LLAR can write to the config directory.

The generated file will have the extension `.llar.example`.
I suggest that you have a look and adjust the generated source keys.
If you want LLAR to load the config, just rename it to `.llar` and it will load the file.

## Features

- Sources: RSS, Atom, Wordpress REST, Reddit, Hacker News, IMAP mailboxes
- Custom Feeds from HTML selectors ([example](https://github.com/irq0/llar-config/blob/main/fefe.llar),
  [example](https://github.com/irq0/llar-config/blob/main/usenixlogin.llar))
- Article extraction (via [Postlight Parser](https://github.com/postlight/parser))
- HTML sanitation (fix URLs, remove annoying elements and ads)
- Bookmark / read it later from single URLs
- Blobstore: Keep copy of feed images to make content self-contained
- Clojure-scriptable processing: Run code to filter or change fetched items
- Reader UI
- Dashboard UI
- Fever-compatible mobile sync for feed-reader clients
- Durable save-for-later capture API for bookmarklets and iOS/macOS Shortcuts

## UI

### Reader

![The LLAR Reader populated with deterministic demo content](https://irq0.github.io/llar/demo/main/reader.png)

Per default running on port 8023.

The same demo deployment also shows the main Reader tools:

| Reading Queue | Continue Reading |
| --- | --- |
| ![LLAR Reading Queue](https://irq0.github.io/llar/demo/main/reading-queue.png) | ![LLAR Continue Reading](https://irq0.github.io/llar/demo/main/continue-reading.png) |

| Gems | Today’s Vibe |
| --- | --- |
| ![LLAR Gems](https://irq0.github.io/llar/demo/main/gems.png) | ![LLAR Today’s Vibe](https://irq0.github.io/llar/demo/main/todays-vibe.png) |

### Dashboard

Per default running on port 9999.

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

The [updater](src/llar/update.clj) [fetches](src/llar/fetch.clj) [sources](src/llar/src.clj),
normalizes the data into *items*, runs them through the [processor](src/llar/postproc.clj)
and finally [persists](src/llar/persistency.clj) them.

`.llar` [files](config/) specify sources to fetch, schedules to run, and much more.
A fetch definition not only contains the [source](src/llar/src.clj), but also pre, post and filter rules, as well as,
UI options and *source tags*.

On update a [fetcher](src/llar/fetch) creates *items*. Each has a title, timestamps, content, descriptions, *tags*, source.
Processors and filters act on individual items. Both are Clojure functions. As long as the result adheres to
the item spec they are free to do any kind of manipulation.

LLAR knows two kinds of tags.
Source tags, that are defined as part of a fetch definition.
They basically group sources and are useful to specify schedules on.
Item tags, that are attached to an item.

The workflow tags are *unread*, *saved*, and *archive*. They are exposed as
semantic actions and states rather than independent tag: saved is independent of
unread, archive is seen and no longer saved, and Done clears saved plus any
active reading checkpoint. *Continue Reading* is a cross-device checkpoint,
not a tag. Captured bookmarks start both saved and unread, and then follow the
same Reading Queue rules as every other item.

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

Paste a source constructor and choose **Run**. Config Lab compiles it, fetches a
snapshot, lists the articles it found, and opens the first item in an isolated
reader-like preview. SelectorFeed articles are fetched on demand. The Selector
and HTTP tabs retain URL matches, per-field extraction results, the raw
response, DOMPurify output, and final LLAR HTML. Selector diagnostics show the
selected Hickory vector passed into each extractor, so extractor functions can
be written against the actual node shape. Clicking a SelectorFeed article opens
an extraction workbench for its title, author, timestamp, description, and
content; the initial automatic selection still opens the reader preview. The
lab keeps this partial diagnostic result when an extracted field has the wrong
type, instead of requiring a valid item first. The Data tab provides an
expandable, EDN-oriented map/vector browser. Keys use keyword notation, long
strings stay collapsed, and nested collections are expanded on demand.
SelectorFeed fetch snapshots also include the exact sanitized Hickory tree used
by the selectors under `:hickory`.

Source key, tags, and reader options live under **Export configuration** because
they identify and configure the deployed source, not the experiment. Export
only copies or downloads a `.llar` form—it never writes runtime config. Refetch
replaces the cached index snapshot, while processor experiments operate on the
session's cached items.

Source and processor forms run through SCI with no filesystem, credential,
arbitrary namespace loading, unrestricted Java interop, or general HTTP access.
The normal `$` helper names resolve for config compatibility, but
`$credentials`, `$http-get`, and `$http-post` remain unavailable rather than
exposing secrets or unguarded requests. Safe value APIs needed by
source forms, currently including `java.net.URI`, are explicitly allowlisted.
Rendered previews cannot run scripts, submit forms, or navigate the dashboard;
images are served only from the owning session's expiring temporary blob store.
The initial version supports unauthenticated `website`, `feed`, `selector-feed`,
`readability`, `hn`, `github-issues`, `github-repos`, and isolated `custom`
sources. Credential-dependent sources and
source implementations whose network calls cannot yet use the lab's redirect
guard are rejected. Fetches reject local/private destinations, run with bounded
time and concurrency, and store generated blobs in the expiring lab session's
temporary directory. The rest of the Dashboard keeps its existing access model;
the token protects only Config Lab.
