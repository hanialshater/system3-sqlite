PRAGMA page_size = 1024; PRAGMA journal_mode = WAL; CREATE TABLE t1(a, b);
INSERT INTO t1 VALUES(1, zeroblob(35*1024))
PRAGMA wal_checkpoint
DELETE FROM t1
VACUUM
PRAGMA wal_checkpoint
INSERT INTO t1 VALUES(2, randomblob(35*1024)); PRAGMA wal_checkpoint; INSERT INTO t1 VALUES(3, randomblob(128)); DELETE FROM t1 WHERE a = 2; VACUUM;
