CREATE TABLE t1(a, b, c); CREATE INDEX i1 ON t1(b); ATTACH 'test.db2' AS aux; CREATE TABLE aux.t2(d, e, f); CREATE INDEX aux.i2 ON t2(e);
SELECT * FROM pragma_index_info('i1')
SELECT * FROM pragma_index_info('i2')
