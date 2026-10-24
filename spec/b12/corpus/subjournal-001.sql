PRAGMA page_count
BEGIN; UPDATE t2 SET a=randomblob(499); SAVEPOINT two; UPDATE t2 SET a=randomblob(498); ROLLBACK TO two; COMMIT; PRAGMA integrity_check;
PRAGMA integrity_check
