PRAGMA journal_mode = memory;
PRAGMA main.journal_mode
PRAGMA journal_mode=persist
ROLLBACK; SELECT * FROM t1;
PRAGMA journal_mode=persist
DROP TABLE IF EXISTS t1; CREATE TABLE t1(x); BEGIN; INSERT INTO t1 VALUES(1);
ROLLBACK
SELECT * FROM t1;
