DROP TABLE IF EXISTS t1; CREATE TABLE t1(a, b, c); CREATE INDEX i1 ON t1(c, b, a);
BEGIN
INSERT INTO t1(c, b, a) VALUES(200, 1, 'a'); INSERT INTO t1(c, b, a) VALUES(200, 1, 'b'); INSERT INTO t1(c, b, a) VALUES(200, 1, 'c'); INSERT INTO t1(c, b, a) VALUES(200, 2, 'e'); INSERT INTO t1(c, b, a) VALUES(200, 2, 'f'); INSERT INTO t1(c, b, a) VALUES(201, 3, 'g'); INSERT INTO t1(c, b, a) VALUES(201, 4, 'h'); ANALYZE; SELECT count(*) FROM sqlite_stat4; SELECT count(*) FROM t1;
SELECT neq, lrange(nlt, 0, 2), lrange(ndlt, 0, 2), lrange(test_decode(sample), 0, 2) FROM sqlite_stat4 ORDER BY rowid LIMIT 16;
SELECT neq, lrange(nlt, 0, 2), lrange(ndlt, 0, 2), lrange(test_decode(sample), 0, 1) FROM sqlite_stat4 ORDER BY rowid DESC LIMIT 2;
SELECT count(DISTINCT c) FROM t1 WHERE c<201
SELECT count(DISTINCT c) FROM t1 WHERE c<200
SELECT count(*) FROM sqlite_stat4 WHERE lindex(test_decode(sample), 3) IN ('34', '68', '102', '136', '170', '204', '238', '272')
