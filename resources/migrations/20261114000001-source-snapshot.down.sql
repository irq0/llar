-- PostgreSQL cannot drop an enum value without rebuilding the type.
-- Keep the additive value and source snapshots on rollback.
SELECT 1;
