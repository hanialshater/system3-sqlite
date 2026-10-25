SELECT json_object('name', 'sqlite', 'year', 2000, 'tags', json_array('embedded', 'sql'));
SELECT json_extract('{"a":{"b":[10,20,30]}}', '$.a.b[1]');
SELECT key, value, type FROM json_each('{"x":1,"y":[2,3],"z":null}');
SELECT json_group_array(n) FROM (SELECT 3 AS n UNION ALL SELECT 1 UNION ALL SELECT 2);
