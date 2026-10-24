CREATE TEMP TABLE t1(a, b); CREATE INDEX i1 ON t1(a, b);
WITH x(i) AS ( SELECT 1 UNION ALL SELECT i+1 FROM x WHERE i<100000 ) INSERT INTO t1 SELECT randomblob(100), randomblob(100) FROM X;
PRAGMA temp.integrity_check;
