-- Migrate sources.data column from hstore to jsonb

-- Convert hstore data to jsonb
ALTER TABLE sources
  ALTER COLUMN data TYPE jsonb
  USING hstore_to_jsonb(data);
