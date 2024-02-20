# 🖖

Live Long and Read!

## Getting Started

LLAR needs a PostgreSQL database, a JVM and a [couple of command line tools](resources/config.edn) to run.
Easiest way to get started is to use docker compose with the [llar container image](https://github.com/irq0/llar/pkgs/container/llar).

```sh
git clone https://github.com/irq0/llar.git
cd llar/docker
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
