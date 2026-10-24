CREATE TABLE t1(a TEXT, b TEXT); INSERT INTO t1 VALUES('(0)', '(0)'); INSERT INTO t1 VALUES('(1)', '(1)'); INSERT INTO t1 VALUES('(2)', '(2)'); INSERT INTO t1 VALUES('(3)', '(3)'); INSERT INTO t1 VALUES('(4)', '(4)'); CREATE INDEX i1 ON t1(a, b);
ANALYZE;
SELECT tbl,idx,nEq,nLt,nDLt,test_decode(sample) FROM sqlite_stat4;
SELECT tbl,idx,nEq,nLt,nDLt,s(sample) FROM sqlite_stat4;
