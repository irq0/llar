CREATE TYPE item_event_type AS ENUM (
    'result-offered',
    'impression',
    'item-opened'
);
--;;
CREATE TYPE item_event_surface AS ENUM (
    'item-detail',
    'related',
    'today-vibe'
);
--;;
CREATE TYPE item_event_trigger AS ENUM (
    'viewport-dwell',
    'item-rendered',
    'open-and-mark-read',
    'related-generated',
    'vibe-generated'
);
--;;
CREATE TABLE item_events (
    id bigserial PRIMARY KEY,
    item_id bigint NOT NULL REFERENCES items(id) ON DELETE CASCADE,
    event_type item_event_type NOT NULL,
    surface item_event_surface NOT NULL,
    trigger item_event_trigger NOT NULL,
    parent_event_id bigint REFERENCES item_events(id) ON DELETE SET NULL,
    position integer CHECK (position IS NULL OR position > 0),
    recorded_at timestamptz NOT NULL DEFAULT now(),
    metadata jsonb NOT NULL DEFAULT '{}'::jsonb,
    data jsonb NOT NULL DEFAULT '{}'::jsonb
);
--;;
CREATE INDEX item_events_item_recorded_idx
    ON item_events (item_id, recorded_at DESC);
--;;
CREATE INDEX item_events_parent_idx ON item_events (parent_event_id);
--;;
CREATE UNIQUE INDEX item_events_one_impression_per_offer_idx
    ON item_events (parent_event_id)
    WHERE event_type = 'impression';
