ATTACH DATABASE 'test2.db' AS aux; PRAGMA aux.auto_vacuum=incremental; CREATE TABLE aux.t2(x); INSERT INTO t2 VALUES(zeroblob(30000)); INSERT INTO t1 SELECT * FROM t2; DELETE FROM t2; DELETE FROM t1;
PRAGMA aux.incremental_vacuum(1)
PRAGMA aux.incremental_vacuum(5)
PRAGMA main.incremental_vacuum(5)
PRAGMA aux.incremental_vacuum
PRAGMA incremental_vacuum(1)
BEGIN; DELETE FROM abc; PRAGMA incremental_vacuum; COMMIT;
