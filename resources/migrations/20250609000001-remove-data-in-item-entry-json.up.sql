UPDATE items
SET entry = entry::jsonb - 'contents' - 'descriptions' - 'thumbs'
WHERE entry::jsonb ? 'contents' OR entry::jsonb ? 'descriptions' OR entry::jsonb ? 'thumbs';
