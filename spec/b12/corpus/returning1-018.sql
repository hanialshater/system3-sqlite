PRAGMA writable_schema=ON; CREATE TABLE xyz (a);
INSERT INTO sqlite_temp_schema DEFAULT VALUES RETURNING (SELECT * FROM xyz AS sqlite_master WHERE a=sqlite_master.name);
