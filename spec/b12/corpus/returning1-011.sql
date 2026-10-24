CREATE TABLE t1(x REAL); INSERT INTO t1(x) VALUES(5.0) RETURNING x, affinity(x);
UPDATE t1 SET x=x+1 RETURNING x, affinity(x);
DELETE FROM t1 RETURNING x, affinity(x);
