CREATE TABLE t1(a TEXT COLLATE binary); CREATE INDEX t1x ON t1(a); INSERT INTO t1(a) VALUES(0),('apple'),(NULL),(''),('banana'); ANALYZE; SELECT format('(%s)',a) FROM t1 WHERE t1.a > CAST(zeroblob(5) AS TEXT);
SELECT format('(%s)',a) FROM t1 WHERE t1.a <= CAST(zeroblob(5) AS TEXT);
