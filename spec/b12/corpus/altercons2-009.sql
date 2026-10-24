CREATE TABLE abc(a, b, c, CONSTRAINT two CHECK (b!=c), d)
CREATE TABLE def(CONSTRAINT abc CHECK( b!=c ), a, b, c);
