PRAGMA legacy_alter_table = 1; ATTACH 'test.db2' AS aux; CREATE TABLE aux.t1(a, b, c); CREATE TABLE main.t1(a, b, c); CREATE TEMP TRIGGER tr AFTER INSERT ON aux.t1 BEGIN SELECT trigger(new.a, new.b, new.c); END;
INSERT INTO main.t1 VALUES(1, 2, 3); INSERT INTO aux.t1 VALUES(4, 5, 6);
SELECT name, tbl_name FROM sqlite_temp_master;
ALTER TABLE main.t1 RENAME TO t2; SELECT name, tbl_name FROM sqlite_temp_master;
ALTER TABLE aux.t1 RENAME TO t2; SELECT name, tbl_name FROM sqlite_temp_master;
INSERT INTO aux.t2 VALUES(7, 8, 9);
