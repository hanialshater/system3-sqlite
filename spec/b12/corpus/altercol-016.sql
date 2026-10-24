CREATE TABLE t1(a INTEGER, b INTEGER PRIMARY KEY, c BLOB, FOREIGN KEY (b) REFERENCES t2)
INSERT INTO t1 VALUES(1, 2, 3);
ALTER TABLE t1 RENAME COLUMN b TO d;
SELECT * FROM t1;
SELECT sql FROM sqlite_master WHERE tbl_name='t1' AND sql!=''
CREATE TABLE t3(a, b, c, d, e, f, g, h, i, j, k, l, m, FOREIGN KEY (b, c, d, e, f, g, h, i, j, k, l, m) REFERENCES t4);
