CREATE TABLE sequence(a INTEGER PRIMARY KEY); INSERT INTO sequence VALUES(1); INSERT INTO sequence VALUES(2);
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
INSERT INTO sequence SELECT a+(SELECT max(a) FROM sequence) FROM sequence;
SELECT count(*) FROM sequence
CREATE TABLE t1(a PRIMARY KEY, b); CREATE TABLE chng(a PRIMARY KEY, b); SELECT count(*) FROM t1; SELECT count(*) FROM chng;
DROP TABLE t1; CREATE TABLE t1(a PRIMARY KEY); SELECT * FROM t1;
