CREATE TABLE t1(a, b, c); CREATE INDEX i1 ON t1(a, b);
INSERT INTO t1 SELECT 'def', b, c FROM t1;
ANALYZE
DROP TABLE IF EXISTS t1; CREATE TABLE t1(i int, x, y, z); INSERT INTO t1 VALUES (1,1,1,1), (2,2,2,2), (3,3,3,3), (4,4,4,4); DROP TABLE IF EXISTS t2; CREATE TABLE t2(i int, bool char); INSERT INTO t2 VALUES(1,'T'), (2,'F'); SELECT count(*) FROM t1 LEFT JOIN t2 ON t1.i=t2.i AND bool='T'; SELECT count(*) FROM t1 LEFT JOIN t2 ON likely(t1.i=t2.i) AND bool='T';
DROP TABLE IF EXISTS t1; CREATE TABLE t1(a, b, PRIMARY KEY(a,b)); INSERT INTO t1 VALUES(9,1),(1,2); DROP TABLE IF EXISTS t2; CREATE TABLE t2(x, y, PRIMARY KEY(x,y)); INSERT INTO t2 VALUES(3,3),(4,4); SELECT likely(a), x FROM t1, t2 ORDER BY 1, 2;
SELECT unlikely(a), x FROM t1, t2 ORDER BY 1, 2;
SELECT likelihood(a,0.5), x FROM t1, t2 ORDER BY 1, 2;
SELECT coalesce(a,a), x FROM t1, t2 ORDER BY 1, 2;
DROP TABLE IF EXISTS t0; CREATE TABLE t0 (c0); INSERT INTO t0(c0) VALUES ('a'); SELECT LIKELY(t0.rowid) <= '0' FROM t0;
SELECT * FROM t0 WHERE LIKELY(t0.rowid) <= '0';
SELECT (t0.rowid) <= '0' FROM t0;
SELECT * FROM t0 WHERE (t0.rowid) <= '0';
SELECT unlikely(t0.rowid) <= '0', likelihood(t0.rowid,0.5) <= '0' FROM t0;
SELECT * FROM t0 WHERE unlikely(t0.rowid) <= '0';
SELECT * FROM t0 WHERE likelihood(t0.rowid, 0.5) <= '0';
SELECT unlikely(t0.rowid <= '0'), likely(t0.rowid <= '0'), likelihood(t0.rowid <= '0',0.5) FROM t0;
SELECT * FROM t0 WHERE unlikely(t0.rowid <= '0');
SELECT * FROM t0 WHERE likelihood(t0.rowid <= '0', 0.5);
