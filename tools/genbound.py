"""genbound.py SEED N : boundary-integer comparisons on rowid / INTEGER PRIMARY KEY / indexed columns."""
import random, sys
seed = int(sys.argv[1]); n = int(sys.argv[2])
R = random.Random(seed)
B = [0, 1, -1, 127, 128, -128, -129, 255, 256, 32767, 32768, -32768, -32769, 65535, 65536, 8388607, 8388608, -8388608, -8388609, 16777215, 16777216,
     2147483647, 2147483648, -2147483648, -2147483649, 4294967295, 4294967296, 140737488355327, 140737488355328, -140737488355328, -140737488355329,
     281474976710655, 281474976710656, 36028797018963967, 36028797018963968, -36028797018963968, -36028797018963969, 9223372036854775807, -9223372036854775808,
     9007199254740992, 9007199254740993, -9007199254740992, -9007199254740993, 4611686018427387904, -4611686018427387904]
vals = sorted(set(R.sample(B, 24)))
lines = []
kind = R.choice(["ipk", "rowid", "wr"])
if kind == "ipk":
    lines.append("CREATE TABLE t1(a INTEGER PRIMARY KEY, x TEXT, r REAL, n NUMERIC, i INT)")
elif kind == "wr":
    lines.append("CREATE TABLE t1(a INTEGER PRIMARY KEY, x TEXT, r REAL, n NUMERIC, i INT) WITHOUT ROWID")
else:
    lines.append("CREATE TABLE t1(a, x TEXT, r REAL, n NUMERIC, i INT)")
for k, v in enumerate(vals):
    if kind == "rowid":
        lines.append("INSERT INTO t1(rowid, a, x, r, n, i) VALUES(%d, %d, '%d', %d, %d, %d)" % (v, v, v, v, v, v))
    else:
        lines.append("INSERT INTO t1(a, x, r, n, i) VALUES(%d, '%d', %d, %d, %d)" % (v, v, v, v, v))
if R.random() < 0.7:
    lines.append("CREATE INDEX t1x ON t1(x)")
if R.random() < 0.7:
    lines.append("CREATE INDEX t1r ON t1(r)")
if R.random() < 0.7:
    lines.append("CREATE INDEX t1n ON t1(n)")
if R.random() < 0.7:
    lines.append("CREATE INDEX t1i ON t1(i)")
OPS = ["=", "<", "<=", ">", ">=", "<>"]


def bv():
    v = R.choice(B)
    k = R.random()
    if k < 0.35:
        return str(v)
    if k < 0.5:
        return repr(float(v))
    if k < 0.6:
        return repr(v + 0.5)
    if k < 0.7:
        return repr(v - 0.5)
    if k < 0.75:
        return "'%d'" % v
    if k < 0.8:
        return "'%d.0'" % v
    if k < 0.85:
        return R.choice(["1e19", "-1e19", "9.3e18", "-9.3e18", "9.2233720368547758e18", "-9.2233720368547758e18", "1e308", "0.5", "'abc'", "NULL", "x'31'", "9223372036854775808", "-9223372036854775809"])
    return "(%d + 1)" % v if v < 9223372036854775807 else str(v)


for _ in range(n):
    c = R.choice(["a", "x", "r", "n", "i", "rowid", "oid"]) if kind != "wr" else R.choice(["a", "x", "r", "n", "i"])
    k = R.random()
    order = R.choice(["ASC", "DESC"])
    if k < 0.6:
        lines.append("SELECT a FROM t1 WHERE %s %s %s ORDER BY %s %s" % (c, R.choice(OPS), bv(), R.choice([c, "a"]) if kind != "wr" else "a", order))
    elif k < 0.75:
        lines.append("SELECT a FROM t1 WHERE %s > %s AND %s < %s ORDER BY a %s" % (c, bv(), c, bv(), order))
    elif k < 0.85:
        lines.append("SELECT a FROM t1 WHERE %s BETWEEN %s AND %s ORDER BY a %s" % (c, bv(), bv(), order))
    elif k < 0.92:
        lines.append("SELECT a FROM t1 WHERE %s IN (%s, %s, %s) ORDER BY a %s" % (c, bv(), bv(), bv(), order))
    else:
        lines.append("SELECT a, %s + %s, %s - %s, %s * %s, typeof(%s + %s) FROM t1 WHERE a IN (%s, %s)" % (c, bv(), c, bv(), c, bv(), c, bv(), R.choice(vals), R.choice(vals)))
print("\n".join(lines))
