CREATE TABLE IF NOT EXISTS annotations (
    id serial PRIMARY KEY,
    item_id bigint NOT NULL REFERENCES items(id) ON DELETE CASCADE,
    selector jsonb,
    body text,
    created_ts timestamp WITHOUT time zone DEFAULT now() NOT NULL,
    CONSTRAINT chk_annotation_content CHECK (selector IS NOT NULL OR body IS NOT NULL)
);
--;;
CREATE INDEX idx_annotations_item_id ON annotations(item_id);
