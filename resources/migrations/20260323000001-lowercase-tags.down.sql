-- Data merge in up migration is irreversible; this only drops the constraint.
ALTER TABLE tags DROP CONSTRAINT IF EXISTS chk_tag_normalized;
