PRAGMA legacy_alter_table = 1; CREATE TABLE t9(a, b, c); CREATE TABLE t10(a, b, c); CREATE TEMP TABLE t9(a, b, c); CREATE TRIGGER temp.t9t AFTER INSERT ON temp.t9 BEGIN INSERT INTO t10 VALUES(new.a, new.b, new.c); END; INSERT INTO temp.t9 VALUES(1, 2, 3); SELECT * FROM t10;
ALTER TABLE temp.t9 RENAME TO 't1234567890'
CREATE TABLE t1(a, b); CREATE TABLE t2(a, b); INSERT INTO t1 VALUES(1, 2); INSERT INTO t2 VALUES(3, 4); CREATE VIEW v AS SELECT one.a, one.b, t2.a, t2.b FROM t1 AS one, t2; SELECT * FROM v;
ALTER TABLE t2 RENAME TO one;
SELECT * FROM v
ALTER TABLE one RENAME TO t2; DROP VIEW v; CREATE VIEW temp.vv AS SELECT one.a, one.b, t2.a, t2.b FROM t1 AS one, t2; SELECT * FROM vv;
ALTER TABLE t2 RENAME TO one;
SELECT * FROM vv
ALTER TABLE x1 RENAME TO x2; SELECT sql FROM sqlite_master WHERE name = 'x2'
CREATE TABLE ddd(db, sql, zOld, zNew, bTemp); INSERT INTO ddd VALUES( 'main', 'CREATE TABLE x1(i INTEGER, t TEXT)', 'ddd', NULL, 0 ), ( 'main', 'CREATE TABLE x1(i INTEGER, t TEXT)', NULL, 'eee', 0 ), ( 'main', NULL, 'ddd', 'eee', 0 );
