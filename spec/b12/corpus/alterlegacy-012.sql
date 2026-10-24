UPDATE mytable SET geom = X'1234'
ALTER TABLE mytable RENAME TO mytable_renamed;
CREATE TRIGGER tr2 AFTER INSERT ON mytable_renamed BEGIN DELETE FROM rt WHERE id=(SELECT min(id) FROM rt); END;
ALTER TABLE mytable_renamed RENAME TO mytable2;
