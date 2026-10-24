CREATE TABLE t1(x); CREATE INDEX i1 ON t1(x,x,x,x,x||2); CREATE INDEX i2 ON t1(1<2); WITH RECURSIVE c(x) AS (VALUES(1) UNION ALL SELECT x+1 FROM c WHERE x<1000) INSERT INTO t1(x) SELECT x FROM c; ANALYZE;
SELECT count(*)>1 FROM sqlite_stat4 WHERE idx='i2' AND neq='1000 1';
SELECT count(*) FROM sqlite_stat4 WHERE idx='i2' AND neq<>'1000 1';
SELECT count(*)>1 FROM sqlite_stat4 WHERE idx='i1' AND neq='1 1 1 1 1 1';
SELECT count(*) FROM sqlite_stat4 WHERE idx='i1' AND neq<>'1 1 1 1 1 1';
