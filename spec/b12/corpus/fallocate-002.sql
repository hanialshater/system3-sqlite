BEGIN ; SELECT count(a) FROM t1
INSERT INTO t1 VALUES(4, randomblob(128)); PRAGMA wal_checkpoint;
SELECT count(b) FROM t1
COMMIT
PRAGMA wal_checkpoint
