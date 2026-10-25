WITH RECURSIVE fib(n, a, b) AS (SELECT 1, 0, 1 UNION ALL SELECT n+1, b, a+b FROM fib WHERE n < 15) SELECT n, a FROM fib;
WITH RECURSIVE cnt(x) AS (SELECT 1 UNION ALL SELECT x+1 FROM cnt LIMIT 10) SELECT group_concat(x*x, ' ') FROM cnt;
CREATE TABLE emp(id INTEGER PRIMARY KEY, name TEXT, boss INT);
INSERT INTO emp VALUES (1,'Ada',NULL),(2,'Bea',1),(3,'Cy',1),(4,'Dee',2),(5,'Eli',4);
WITH RECURSIVE chain(id, name, depth) AS (SELECT id, name, 0 FROM emp WHERE boss IS NULL UNION ALL SELECT e.id, e.name, depth+1 FROM emp e JOIN chain c ON e.boss = c.id) SELECT substr('        ', 1, depth*2) || name AS tree FROM chain ORDER BY depth, id;
