-- :name create-item-event :<! :1
INSERT INTO item_events
  (item_id, event_type, surface, trigger,
   parent_event_id, position, metadata, data)
VALUES
  (:item-id, :event-type, :surface, :trigger,
   :parent-event-id, :position, :metadata, :data)
RETURNING id, item_id AS "item-id", event_type AS "event-type",
          surface, trigger,
          parent_event_id AS "parent-event-id",
          position, recorded_at, metadata, data

-- :name get-item-event :? :1
SELECT id, item_id AS "item-id", event_type AS "event-type",
       surface, trigger,
       parent_event_id AS "parent-event-id",
       position, recorded_at, metadata, data
FROM item_events WHERE id = :id

-- :name get-item-events :? :*
SELECT e.id, e.item_id AS "item-id", e.event_type AS "event-type",
       e.surface, e.trigger,
       e.parent_event_id AS "parent-event-id", e.position,
       e.recorded_at, e.metadata, e.data
FROM item_events e
WHERE e.item_id = :item-id
ORDER BY e.recorded_at, e.id

-- :name record-impression-for-offer :<! :1
INSERT INTO item_events
  (item_id, event_type, surface, trigger,
   parent_event_id, metadata, data)
SELECT item_id, 'impression', surface, 'viewport-dwell', id, metadata, :data
FROM item_events
WHERE id = :offered-event-id AND event_type = 'result-offered'
ON CONFLICT (parent_event_id) WHERE event_type = 'impression' DO NOTHING
RETURNING id, item_id AS "item-id", event_type AS "event-type",
          surface, trigger,
          parent_event_id AS "parent-event-id", position, recorded_at,
          metadata, data
