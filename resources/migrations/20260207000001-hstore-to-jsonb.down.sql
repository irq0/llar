-- Rollback: Convert sources.data column from jsonb back to hstore

-- Convert jsonb data back to hstore
ALTER TABLE sources
  ALTER COLUMN data TYPE hstore
  USING hstore(data);
