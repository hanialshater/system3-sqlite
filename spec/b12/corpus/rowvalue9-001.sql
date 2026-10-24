CREATE TABLE t1(a, b); INSERT INTO t1 VALUES(1, 1), (1, 2), (2, 2), (2, 3), (3, 3), (3, 4), (4, 4);
SELECT * FROM t1 WHERE (a, b) IN ( (3, 3), (2, 2) );
CREATE INDEX i1 ON t1(a);
SELECT * FROM t1 WHERE (a, b) IN ( (3, 3), (2, 2) );
CREATE INDEX i2 ON t1(b, a); SELECT * FROM t1 WHERE (a, b) IN ( (3, 3), (2, 2) );
