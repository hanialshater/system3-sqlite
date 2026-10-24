CREATE TABLE x1(a INTEGER PRIMARY KEY, b) WITHOUT ROWID; INSERT INTO x1 VALUES (1, 'one'), (2, 'two'), (3, 'three'), (4, 'four'), (5, 'five'), (6, 'six'), (7, 'seven'), (8, 'eight');
CREATE TABLE data1(x, y); INSERT INTO data1 VALUES (1, 'eleven'), (1, 'twenty-one'), (2, 'twelve'), (2, 'twenty-two'), (3, 'thirteen'), (3, 'twenty-three'), (4, 'fourteen'), (4, 'twenty-four');
UPDATE x1 SET b=y FROM data1 WHERE a=x ORDER BY a LIMIT 3; SELECT * FROM x1;
UPDATE x1 SET b=b||y FROM data1 WHERE a=x ORDER BY b LIMIT 3; SELECT * FROM x1;
UPDATE x1 SET b=b||b ORDER BY b;
UPDATE x1 SET b=b||y FROM data1 WHERE a=x ORDER BY b;
DROP TABLE x1; CREATE TABLE x1(u, v, b, PRIMARY KEY(u, v)) WITHOUT ROWID; INSERT INTO x1 VALUES (0, 1, 'one'), (1, 0, 'two'), (1, 1, 'three'), (2, 0, 'four'), (2, 1, 'five'), (3, 0, 'six'), (3, 1, 'seven'), (4, 0, 'eight');
UPDATE x1 SET b=y FROM data1 WHERE (u*2+v)=x ORDER BY u, v LIMIT 3; SELECT * FROM x1;
UPDATE x1 SET b=b||y FROM data1 WHERE (u*2+v)=x ORDER BY b LIMIT 3; SELECT * FROM x1;
