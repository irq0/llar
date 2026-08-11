# LLAR demo environment

This directory defines deterministic, network-free content for UI development,
CI, screenshots, and the future introductory video. It is deliberately
separate from the ordinary example configuration.

From the repository root, start it with the normal Compose file plus the demo
overlay:

```sh
docker compose \
  -f docker/docker-compose.yaml \
  -f docker/docker-compose.demo.yaml \
  up -d
```

After the application is healthy, populate the clean database immediately:

```sh
curl -fsS -X POST http://localhost:9999/api/schedule/demo-sources/run
```

To populate Today’s Vibe immediately as well:

```sh
curl -fsS -X POST http://localhost:9999/api/schedule/update-todays-vibe/run
```

The source uses a fixed seed for generated names and filler text. LLAR owns the
small set of editorial themes and headline angles so Today’s Vibe has coherent
cross-source stories rather than unrelated Lorem Ipsum. All preview artwork is
served locally; the demo does not depend on live feeds or third-party images.

The media pipeline will reuse this environment in three stages:

1. Compose smoke test and content verification.
2. [Playwright screenshots](media/README.md) at fixed viewport, locale, theme,
   and font state.
3. A short Playwright journey recorded from the same seeded database.
