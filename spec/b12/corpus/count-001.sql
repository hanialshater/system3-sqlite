CREATE TABLE t1(a INTEGER PRIMARY KEY, b INT, c VARCHAR(1000)); CREATE INDEX t1b ON t1(b); INSERT INTO t1(a,b,c) values(1,2,'count.test cases for NOT INDEXED'); ANALYZE; UPDATE sqlite_stat1 SET stat='1000000 10' WHERE idx='t1b'; ANALYZE sqlite_master;
CREATE TABLE t7(a INT,b TEXT,c BLOB,d REAL); CREATE TABLE t8(a INT,b TEXT,c BLOB,d REAL); CREATE INDEX t8a ON t8(a);
SELECT * FROM t8 WHERE (a, b) IN ( SELECT count(t8.b), count(*) FROM t7 AS ra0 ORDER BY count(*) ) AND t8.b=0;
