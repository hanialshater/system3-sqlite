SELECT load_extension('../dbdata')
CREATE TABLE t1(a); CREATE INDEX i1 ON t1(a); WITH s(i) AS ( SELECT 1 UNION ALL SELECT i+1 FROM s WHERE i<10 ) INSERT INTO t1 SELECT randomblob(900) FROM s;
SELECT * FROM sqlite_dbptr WHERE pgno=2;
SELECT * FROM sqlite_dbptr WHERE pgno=3;
SELECT * FROM sqlite_dbptr
