DROP TABLE IF EXISTS t1; DROP TABLE IF EXISTS t2; CREATE TABLE t1(a INT PRIMARY KEY, b INT); CREATE TABLE t2(c INT PRIMARY KEY, d INT REFERENCES t1); SELECT t.name, f."table", f."from", i.name, i.pk FROM pragma_table_list() AS t JOIN pragma_foreign_key_list(t.name, t.schema) AS f JOIN pragma_table_info(f."table", t.schema) AS i WHERE i.pk;
CREATE VIEW v1 AS SELECT abs(a) FROM t1; PRAGMA writable_schema=ON; UPDATE sqlite_schema SET sql=replace(sql,'abs(a)','nosuchfunc(a)') WHERE name='v1'; PRAGMA writable_schema=RESET;
PRAGMA table_list;
CREATE TABLE t3 ("a" TEXT, "b" TEXT); CREATE TABLE t4 ("a" TEXT, "b" TEXT, "c" TEXT);
CREATE TABLE pragma_t3 AS SELECT * FROM pragma_table_info('t3'); CREATE TABLE pragma_t4 AS SELECT * FROM pragma_table_info('t4');
SELECT pragma_t4.name, pragma_t3.name FROM pragma_t4 RIGHT JOIN pragma_t3 ON (pragma_t4.name=pragma_t3.name);
SELECT t4.name, t3.name FROM pragma_table_info('t4') t4 RIGHT JOIN pragma_table_info('t3') t3 ON (t4.name=t3.name);
