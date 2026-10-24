PRAGMA legacy_alter_table = 1; CREATE TABLE txx(a, b, c); INSERT INTO txx VALUES(1, 2, 3); CREATE VIEW vvv AS SELECT main.txx.a, txx.b, c FROM txx; CREATE VIEW uuu AS SELECT main.one.a, one.b, c FROM txx AS one; CREATE VIEW temp.ttt AS SELECT main.txx.a, txx.b, one.b, main.one.a FROM txx AS one, txx;
SELECT * FROM vvv;
ALTER TABLE txx RENAME TO "t xx";
SELECT * FROM vvv;
SELECT sql FROM sqlite_master WHERE name='vvv';
SELECT * FROM uuu;
SELECT sql FROM sqlite_master WHERE name='uuu';;
SELECT * FROM ttt;
SELECT sql FROM sqlite_temp_master WHERE name='ttt';
