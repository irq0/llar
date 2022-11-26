INSERT INTO tags VALUES
       (0, 'unread'),
       (1, 'saved'),
       (2, 'in-progress'),
       (3, 'archive');
--;;
ALTER SEQUENCE tags_id_seq RESTART WITH 100;
