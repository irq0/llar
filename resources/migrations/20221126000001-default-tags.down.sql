DELETE FROM tags WHERE
   (id = 0 AND tag = 'unread')
 OR
   (id = 1 AND tag = 'saved')
 OR
   (id = 2 AND tag = 'in-progress')
 OR
   (id = 3 AND tag = 'archive');
