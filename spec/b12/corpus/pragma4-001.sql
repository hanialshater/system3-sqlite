CREATE TABLE t1(a, b, c); ATTACH 'test.db2' AS aux; CREATE TABLE aux.t2(d, e, f);
PRAGMA table_info = t1
PRAGMA table_info = t2
