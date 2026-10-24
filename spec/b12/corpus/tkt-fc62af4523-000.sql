PRAGMA cache_size = 10; PRAGMA journal_mode = persist; CREATE TABLE t1(a UNIQUE, b UNIQUE); INSERT INTO t1 SELECT randomblob(200), randomblob(300); INSERT INTO t1 SELECT randomblob(200), randomblob(300) FROM t1; INSERT INTO t1 SELECT randomblob(200), randomblob(300) FROM t1; INSERT INTO t1 SELECT randomblob(200), randomblob(300) FROM t1; INSERT INTO t1 SELECT randomblob(200), randomblob(300) FROM t1; INSERT INTO t1 SELECT randomblob(200), randomblob(300) FROM t1; INSERT INTO t1 SELECT randomblob(200), randomblob(300) FROM t1;
PRAGMA integrity_check; SELECT count(*) FROM t1;
PRAGMA journal_mode = DELETE
PRAGMA integrity_check; SELECT count(*) FROM t1;
