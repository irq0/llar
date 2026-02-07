-- Drop tables (in reverse order of creation due to foreign keys)
DROP TABLE IF EXISTS item_data CASCADE;
--;;
DROP TABLE IF EXISTS items CASCADE;
--;;
DROP TABLE IF EXISTS tags CASCADE;
--;;
DROP TABLE IF EXISTS sources CASCADE;
--;;

-- Drop enums
DROP TYPE IF EXISTS item_type CASCADE;
--;;
DROP TYPE IF EXISTS item_data_type CASCADE;
--;;

-- Drop extensions
DROP EXTENSION IF EXISTS intarray;
--;;
DROP EXTENSION IF EXISTS unaccent;
--;;
DROP EXTENSION IF EXISTS pg_trgm;
--;;
DROP EXTENSION IF EXISTS hstore;
--;;
DROP EXTENSION IF EXISTS btree_gist;
--;;
DROP EXTENSION IF EXISTS btree_gin;
--;;
