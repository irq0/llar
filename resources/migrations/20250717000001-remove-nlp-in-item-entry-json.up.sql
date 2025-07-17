UPDATE items
SET entry = entry::jsonb - 'nlp'
WHERE entry::jsonb ? 'nlp'
