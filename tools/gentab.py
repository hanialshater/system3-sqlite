"""gentab.py SEED N : random tables with declared types, random values, random queries (one file to stdout)."""
import random, sys
seed = int(sys.argv[1]); n = int(sys.argv[2])
R = random.Random(seed)
TYPES = ["INT", "INTEGER", "TEXT", "REAL", "NUMERIC", "BLOB", "", "FLOAT", "CHAR(5)", "DOUBLE", "VARCHAR(10)", "BOOLEAN", "DATE", "DECIMAL(5,2)", "CLOB", "BIGINT", "TEXTUAL", "NUM", "REEL", "POINT", "INTEGER PRIMARY KEY"]
VALS = ["0", "1", "-1", "2", "10", "100", "9223372036854775807", "-9223372036854775808", "0.0", "0.5", "1.5", "-2.5", "1e3", "1e20", "1.0", "3.0",
        "'0'", "'1'", "'2'", "'10'", "'1.0'", "'1.5'", "'1e3'", "' 1'", "'1 '", "'abc'", "'ABC'", "'a'", "'b'", "''", "'0x10'", "'+1'", "'-1'", "'9223372036854775808'", "'1.5abc'",
        "NULL", "x''", "x'31'", "x'41'", "x'00'", "'true'", "TRUE", "FALSE", "'2020-01-01'", "'.5'", "'1e'", "'e1'", "'12abc'"]
COLL = ["", "", "", " COLLATE NOCASE", " COLLATE RTRIM", " COLLATE BINARY"]
ncols = R.randint(2, 4)
cols = []
for i in range(ncols):
    t = R.choice(TYPES[:-1])
    cols.append((("c%d" % i), t, R.choice(COLL)))
lines = []
lines.append("CREATE TABLE t1(" + ", ".join("%s %s%s" % (c, t, co) for c, t, co in cols) + ")")
lines.append("CREATE TABLE t2(" + ", ".join("%s %s%s" % (c, R.choice(TYPES[:-1]), R.choice(COLL)) for c, t, co in cols) + ")")
for tb in ("t1", "t2"):
    for _ in range(R.randint(6, 14)):
        lines.append("INSERT INTO %s VALUES(%s)" % (tb, ", ".join(R.choice(VALS) for _ in cols)))
OPS = ["=", "<", ">", "<=", ">=", "<>", "IS", "IS NOT"]


def term():
    c = R.choice(cols)[0]
    k = R.random()
    if k < 0.45:
        return "%s %s %s" % (c, R.choice(OPS), R.choice(VALS))
    if k < 0.6:
        return "%s %s %s" % (c, R.choice(OPS), R.choice(cols)[0])
    if k < 0.7:
        return "%s IN (%s)" % (c, ", ".join(R.choice(VALS) for _ in range(R.randint(1, 3))))
    if k < 0.78:
        return "%s BETWEEN %s AND %s" % (c, R.choice(VALS), R.choice(VALS))
    if k < 0.84:
        return "%s IN (SELECT %s FROM t2)" % (c, R.choice(cols)[0])
    if k < 0.9:
        return "%s %s (SELECT max(%s) FROM t2)" % (c, R.choice(OPS), R.choice(cols)[0])
    if k < 0.95:
        return "+%s %s %s" % (c, R.choice(OPS), R.choice(VALS))
    return "%s %s CAST(%s AS %s)" % (c, R.choice(OPS), R.choice(VALS), R.choice(["TEXT", "INTEGER", "REAL", "NUMERIC", "BLOB"]))


def where():
    n = R.randint(1, 2)
    ts = [term() for _ in range(n)]
    return " WHERE " + (" AND " if R.random() < 0.7 else " OR ").join(ts)


def query():
    c = R.choice(cols)[0]
    k = R.random()
    if k < 0.45:
        return "SELECT rowid, %s FROM t1%s ORDER BY rowid" % (c, where())
    if k < 0.55:
        return "SELECT %s, typeof(%s), count(*) FROM t1 GROUP BY %s ORDER BY 1, 2" % (c, c, c)
    if k < 0.62:
        return "SELECT DISTINCT %s FROM t1 ORDER BY 1" % c
    if k < 0.70:
        return "SELECT %s, typeof(%s) FROM t1 ORDER BY %s%s, rowid" % (c, c, c, R.choice(["", " DESC"]))
    if k < 0.76:
        return "SELECT min(%s), max(%s), sum(%s), total(%s), count(%s), group_concat(%s) FROM t1%s" % ((c,) * 6 + (where() if R.random() < 0.4 else "",))
    if k < 0.84:
        return "SELECT t1.rowid, t2.rowid FROM t1 JOIN t2 ON t1.%s %s t2.%s ORDER BY 1, 2" % (c, R.choice(OPS), R.choice(cols)[0])
    if k < 0.9:
        return "SELECT rowid FROM t1 WHERE %s %s (SELECT %s FROM t2 ORDER BY rowid LIMIT 1) ORDER BY 1" % (c, R.choice(OPS), R.choice(cols)[0])
    if k < 0.95:
        return "SELECT %s %s %s FROM t1 ORDER BY rowid" % (c, R.choice(OPS), R.choice(VALS))
    return "SELECT rowid, %s FROM t1 WHERE %s ORDER BY %s, rowid" % (c, term(), R.choice(cols)[0])


def index_stmt():
    c = R.choice(cols)[0]
    return "CREATE INDEX i%d ON %s(%s%s)" % (R.randint(1, 999), R.choice(["t1", "t1", "t2"]), c, R.choice(["", "", " DESC", " COLLATE NOCASE"]))


for _ in range(n):
    r = R.random()
    if r < 0.05:
        lines.append(index_stmt())
    elif r < 0.08:
        lines.append("UPDATE t1 SET %s = %s WHERE rowid = %d" % (R.choice(cols)[0], R.choice(VALS), R.randint(1, 8)))
    else:
        lines.append(query())
print("\n".join(lines))
