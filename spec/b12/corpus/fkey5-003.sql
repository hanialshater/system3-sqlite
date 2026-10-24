ATTACH ':memory:' as aux; CREATE TABLE aux.t1(a INTEGER PRIMARY KEY, b TEXT REFERENCES t2); CREATE TABLE main.t2(x TEXT PRIMARY KEY, y INT); INSERT INTO main.t2 VALUES('abc',11),('def',22),('xyz',99); INSERT INTO aux.t1 VALUES(5,'abc'),(7,'xyz'),(9,'oops'); PRAGMA foreign_key_check=t1;
CREATE TABLE aux.t2(x TEXT PRIMARY KEY, y INT); INSERT INTO aux.t2 VALUES('abc',11),('def',22),('xyz',99); PRAGMA foreign_key_check=t1;
SELECT *, 'x' FROM pragma_foreign_key_check('t1');
SELECT *, 'x' FROM pragma_foreign_key_check('t1','main');
SELECT *, 'x' FROM pragma_foreign_key_check('t1','aux');
