INSERT INTO t1(a) VALUES('xyzzy'); SELECT a, quote(b) FROM t1;
CREATE TABLE t2(a TEXT, b TEXT DEFAULT(:xyz));
CREATE TABLE t2(a TEXT, b TEXT DEFAULT(abs(:xyz)));
CREATE TABLE t2(a TEXT, b TEXT DEFAULT(98+coalesce(5,:xyz)));
