CREATE TABLE t(i INTEGER, r REAL, s TEXT, b BLOB, n);
INSERT INTO t VALUES ('42', '3', 17, x'cafe', '1e3');
SELECT typeof(i), typeof(r), typeof(s), typeof(b), typeof(n), i, r, s, n FROM t;
SELECT 1/2, 1.0/2, 7%3, '10' + 5, '3 apples' + 1, NULL + 1;
SELECT 2 < '10', '2' < '10', 'abc' < 'abd', x'00' < 'a';
