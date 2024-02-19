# 🖖

Live Long and Read!

## Getting Started

LLAR needs a PostgreSQL database, a JVM and a [couple of command line tools](resources/config.edn) to run.
Easiest way to get started is to use docker compose with the [llar container image](https://github.com/irq0/llar/pkgs/container/llar).

```sh
https://github.com/irq0/llar.git
cd llar/docker
docker-compose up
```

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

## UI

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

### Reader

Per default running on port 8023.

The main feed reader interface :)

## Concept

The [updater](src/llar/update.clj) [fetches](src/llar/fetch.clj) [sources](src/llar/src.clj),
runs them through the [processor](src/llar/postproc.clj) and
[persists](src/llar/persistency.clj) them.

`.llar` [files](config/) specify sources to fetch, schedules to run, and much more.
A fetch definition not only contains the [source](src/llar/src.clj), but also pre, post and filter rules.
UI options and *source tags*.

On update a [fetcher](src/llar/fetch) creates *items*. Each has a title, timestamps, content, descriptions, *tags*, source.
Processors and filters act on individual items and modify them to
their liking as long as they still adhere to the item spec afterwards.

LLAR knows two kinds of tags.
Source tags that are part of a fetch definition and group sources.
Item tags that are part of an item.
Predefined tags are *unread*, *saved*, *in-progress*, *archive*, *highlight*.
Each has its own semantics in the UI. Arbitrary additional tags are supported.

## Configuration

### Appconfig

Loaded on startup. Contains paths to command line tools, blob store,
state directory, credentials file.

Examples:

- [config.edn](resources/config.edn)
- [docker-config.edn](docker/docker-config.edn)

### Config

Runtime configuration. Automatically loaded when files in the `:runtime-config-dir` change.
Specify sources to fetch, schedules, highlight rules here.

Files are (almost) Clojure code with extra constructs for convenience. They use the extension `.llar`.
See [readme.llar](config/readme.llar) for documentation
or check out [my config](https://github.com/irq0/llar-config).

### Credentials

The credentials file contains secrets made available with the `$credentials` function in `.llar` config files.
See [credentials.edn.example](resources/credentials.edn.example).
