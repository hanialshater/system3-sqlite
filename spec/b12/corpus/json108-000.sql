CREATE TEMP TABLE t1(j0,j5); WITH RECURSIVE c(n) AS (VALUES(0) UNION ALL SELECT n+1 FROM c WHERE n<9) INSERT INTO t1 SELECT random_json(n), random_json5(n) FROM c;
SELECT count(*) FROM t1 WHERE json(j0)==json(json_pretty(j0,NULL));
SELECT count(*) FROM t1 WHERE json(j0)==json(json_pretty(j0,NULL));
SELECT count(*) FROM t1 WHERE json(j0)==json(json_pretty(j0,''));
SELECT count(*) FROM t1 WHERE json(j0)==json(json_pretty(j0,char(9)));
SELECT count(*) FROM t1 WHERE json(j0)==json(json_pretty(j0,'/*hello*/'));
