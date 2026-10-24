PRAGMA trusted_schema=OFF
CREATE TABLE t1(x INT, y TEXT); INSERT INTO t1 VALUES(1,'abc'),(2,'ABC'),(3,'Abc'); CREATE VIEW t2 AS SELECT * FROM t1 WHERE y LIKE 'a%'; SELECT * FROM t2;
PRAGMA case_sensitive_like=OFF; SELECT * FROM t2;
PRAGMA case_sensitive_like=ON; SELECT * FROM t2;
