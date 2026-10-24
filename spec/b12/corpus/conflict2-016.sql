pragma temp_store=file
DROP TABLE t1; CREATE TABLE t1(a,b,c, PRIMARY KEY(a) ) WITHOUT rowid; INSERT INTO t1 SELECT * FROM t2; UPDATE t3 SET x=0; BEGIN; UPDATE OR ROLLBACK t3 SET x=1; UPDATE OR ROLLBACK t1 SET b=b*2; UPDATE OR ROLLBACK t1 SET a=c+5;
COMMIT
SELECT a FROM t1 ORDER BY b
SELECT x FROM t3
DROP TABLE t1; DROP TABLE t2; DROP TABLE t3; CREATE TABLE t1(a PRIMARY KEY, b) without rowid;
INSERT into t1 values(1,2);
INSERT into t1 values(2,3);
INSERT into t1 values(3,4);
INSERT into t1 values(4,5);
INSERT into t1 values(5,6);
INSERT into t1 values(6,7);
INSERT into t1 values(7,8);
INSERT into t1 values(8,9);
INSERT into t1 values(9,10);
INSERT into t1 values(10,11);
INSERT into t1 values(11,12);
INSERT into t1 values(12,13);
INSERT into t1 values(13,14);
INSERT into t1 values(14,15);
INSERT into t1 values(15,16);
INSERT into t1 values(16,17);
INSERT into t1 values(17,18);
INSERT into t1 values(18,19);
INSERT into t1 values(19,20);
INSERT into t1 values(20,21);
INSERT into t1 values(21,22);
