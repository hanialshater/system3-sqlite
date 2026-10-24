CREATE TABLE t1(a REFERENCES sqlite_stat1 ON DELETE CASCADE); CREATE TABLE t2(a TEXT PRIMARY KEY); PRAGMA writable_schema=ON; CREATE TABLE sqlite_stat1(tbl INTEGER PRIMARY KEY DESC, idx UNIQUE DEFAULT NULL) WITHOUT ROWID; UPDATE sqlite_schema SET name='sqlite_autoindex_sqlite_stat1_1' WHERE name='sqlite_autoindex_sqlite_stat1_2'; PRAGMA writable_schema=RESET;
REINDEX;
