CREATE TABLE "Test" ( "IsActive" INTEGER, CONSTRAINT "BooleanZeroOrOne" CHECK ("IsActive" IN (0, 1)) );
ALTER TABLE Test DROP CONSTRAINT BooleanZeroOrOne
SELECT sql FROM sqlite_schema
DROP TABLE Test; CREATE TABLE "Test" ( "IsActive" INTEGER, CONSTRAINT "BooleanZeroOrOne" CHECK ("IsActive" IN (0, 1)) );
ALTER TABLE Test DROP CONSTRAINT "BooleanZeroOrOne"
SELECT sql FROM sqlite_schema
CREATE TABLE t1(a, b CONSTRAINT "a""b" CHECK (b IS NOT NULL));
ALTER TABLE t1 DROP CONSTRAINT "a""b"
