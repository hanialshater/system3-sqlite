DROP INDEX i1
DROP INDEX i2
SELECT * FROM pragma_index_info('i1')
SELECT * FROM pragma_index_info('i2')
SELECT * FROM main.sqlite_master, aux.sqlite_master
CREATE INDEX main.i1 ON t1(b, c); CREATE INDEX aux.i2 ON t2(e, f);
SELECT * FROM pragma_index_list('t1')
SELECT * FROM pragma_index_list('t2')
DROP INDEX i1
DROP INDEX i2
SELECT * FROM pragma_index_list('t1')
SELECT * FROM pragma_index_list('t2')
SELECT * FROM main.sqlite_master, aux.sqlite_master
CREATE UNIQUE INDEX main.i1 ON t1(a); CREATE UNIQUE INDEX aux.i2 ON t2(d); CREATE TABLE main.c1 (a, b, c REFERENCES t1(a)); CREATE TABLE aux.c2 (d, e, r REFERENCES t2(d));
SELECT * FROM pragma_foreign_key_list('c1')
SELECT * FROM pragma_foreign_key_list('c2')
DROP TABLE c1
DROP TABLE c2
SELECT * FROM pragma_foreign_key_list('c1')
SELECT * FROM pragma_foreign_key_list('c2')
SELECT * FROM main.sqlite_master, aux.sqlite_master
CREATE TABLE main.c1 (a, b, c REFERENCES t1(a)); CREATE TABLE aux.c2 (d, e, r REFERENCES t2(d)); INSERT INTO main.c1 VALUES(1, 2, 3); INSERT INTO aux.c2 VALUES(4, 5, 6);
pragma foreign_key_check('c1')
pragma foreign_key_check('c2')
DROP TABLE c2
pragma foreign_key_check('c1')
pragma foreign_key_check('c2')
CREATE TABLE t4(a DEFAULT 'abc' , b DEFAULT -1 , c DEFAULT +4.0 ); PRAGMA table_info = t4;
