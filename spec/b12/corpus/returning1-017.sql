PRAGMA writable_schema=ON; INSERT INTO sqlite_schema DEFAULT VALUES RETURNING sqlite_schema.name;
INSERT INTO sqlite_temp_schema DEFAULT VALUES RETURNING sqlite_temp_schema.name;
