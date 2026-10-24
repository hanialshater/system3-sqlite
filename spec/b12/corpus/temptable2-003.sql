PRAGMA main.cache_size = 10; PRAGMA temp.cache_size = 10; CREATE TEMP TABLE t1(a, b); CREATE INDEX i1 ON t1(a, b); WITH x(i) AS ( SELECT 1 UNION ALL SELECT i+1 FROM x WHERE i<10 ) INSERT INTO t1 SELECT randomblob(100), randomblob(100) FROM x; SELECT count(*) FROM t1; PRAGMA temp.page_count;
BEGIN; UPDATE t1 SET b=randomblob(100); ROLLBACK;
CREATE TEMP TABLE t2(a, b); CREATE INDEX i2 ON t2(a, b); WITH x(i) AS ( SELECT 1 UNION ALL SELECT i+1 FROM x WHERE i<500 ) INSERT INTO t2 SELECT randomblob(100), randomblob(100) FROM x; SELECT count(*) FROM t2; SELECT count(*) FROM t1;
PRAGMA temp.page_count
PRAGMA temp.integrity_check
