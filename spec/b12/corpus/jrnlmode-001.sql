CREATE TABLE x(n INTEGER); ATTACH 'test2.db' AS a; create table a.x ( n integer ); insert into a.x values(1); insert into a.x values (2); insert into a.x values (3); insert into a.x values (4);
PRAGMA journal_mode=off;
BEGIN IMMEDIATE; INSERT OR IGNORE INTO main.x SELECT * FROM a.x; COMMIT;
