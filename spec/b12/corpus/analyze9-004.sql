BEGIN; CREATE TABLE t1(o,t INTEGER PRIMARY KEY); CREATE INDEX i1 ON t1(o);
COMMIT; ANALYZE; SELECT count(*) FROM sqlite_stat4;
SELECT test_decode(sample) FROM sqlite_stat4;
