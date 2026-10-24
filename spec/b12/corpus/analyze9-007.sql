CREATE TABLE t1(a, b); CREATE INDEX i1 ON t1(a, b); INSERT INTO t1 VALUES(1, 1); INSERT INTO t1 VALUES(2, 2); INSERT INTO t1 VALUES(3, 3); INSERT INTO t1 VALUES(4, 4); INSERT INTO t1 VALUES(5, 5); ANALYZE; UPDATE sqlite_stat4 SET sample = X'' WHERE rowid = 1; ANALYZE sqlite_master;
UPDATE sqlite_stat4 SET sample = X'FFFF'; ANALYZE sqlite_master; SELECT * FROM t1 WHERE a = 1;
ANALYZE; UPDATE sqlite_stat4 SET neq = '0 0 0'; ANALYZE sqlite_master; SELECT * FROM t1 WHERE a = 1;
ANALYZE; UPDATE sqlite_stat4 SET ndlt = '0 0 0'; ANALYZE sqlite_master; SELECT * FROM t1 WHERE a = 3;
ANALYZE; UPDATE sqlite_stat4 SET nlt = '0 0 0'; ANALYZE sqlite_master; SELECT * FROM t1 WHERE a = 5;
