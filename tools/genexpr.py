"""genexpr.py SEED N > out.sql : random scalar expressions, one SELECT per line (for tools/probe.py)."""
import random, sys
seed = int(sys.argv[1]); n = int(sys.argv[2])
R = random.Random(seed)
LITS = ["0", "1", "-1", "2", "3", "7", "10", "255", "-255", "65535", "2147483647", "2147483648", "-2147483649",
        "9223372036854775807", "-9223372036854775808", "9223372036854775808", "0.0", "-0.0", "0.5", "1.5", "-1.5", "2.5",
        "1e0", "1e10", "1e15", "1e16", "1e18", "1e19", "1e-5", "1.7976931348623157e308", "5e-324", "0.1", "0.3", "123456789.123456789",
        "'0'", "'1'", "'-1'", "'1.5'", "'1e3'", "' 12 '", "'12abc'", "'abc'", "'ABC'", "''", "'0x1F'", "'+5'", "'.5'", "'5.'", "'1e'", "'-'", "'  '",
        "'a b'", "'é'", "'日本'", "'%'", "'_'", "'aBc'", "'Hello World'", "'9223372036854775807'", "'9223372036854775808'", "'-0'", "'1_000'",
        "NULL", "NULL", "x''", "x'00'", "x'41'", "x'4142'", "x'ff'", "x'31'", "x'3132'", "TRUE", "FALSE", "'true'", "CURRENT_TIMESTAMP IS NULL"]
UN = ["-", "+", "NOT ", "~"]
BIN = ["+", "-", "*", "/", "%", "||", "<", "<=", ">", ">=", "=", "==", "!=", "<>", "IS", "IS NOT", "AND", "OR", "&", "|", "<<", ">>",
       "LIKE", "GLOB", "IS DISTINCT FROM", "IS NOT DISTINCT FROM"]
F1 = ["abs", "typeof", "length", "upper", "lower", "hex", "quote", "unicode", "trim", "ltrim", "rtrim", "round", "sign", "octet_length",
      "unhex", "likely", "unlikely", "randomblob2", "zeroblob", "char", "json_quote", "json_valid", "json_type", "json", "date", "time", "datetime", "julianday", "unixepoch", "total", "printf", "format", "subtype"]
F2 = ["round", "substr", "instr", "trim", "ltrim", "rtrim", "nullif", "ifnull", "coalesce", "max", "min", "replace2", "printf", "format", "json_extract", "glob", "like", "unhex", "strftime", "timediff", "concat", "concat_ws", "char", "date", "iif2"]
F3 = ["substr", "replace", "coalesce", "iif", "max", "min", "printf", "strftime", "concat_ws", "json_set", "json_insert", "like", "char", "nullif3", "date", "datetime"]
CASTS = ["INTEGER", "REAL", "TEXT", "BLOB", "NUMERIC", "INT", "FLOAT", "CHAR(3)", "UNSIGNED BIG INT"]
FMTS = ["'%d'", "'%s'", "'%5.2f'", "'%x'", "'%c'", "'%q'", "'%Q'", "'%e'", "'%g'", "'%05d'", "'%-5s|'", "'%+d'", "'%.3s'", "'%o'", "'%X'", "'%u'", "'%i'", "'%!.5f'", "'%w'", "'%,d'", "'%lld'", "'%#x'", "'%10.4e'", "'%G'", "'%%'", "'%z'", "'%p'", "'%b'"]
STRF = ["'%Y-%m-%d'", "'%s'", "'%J'", "'%w'", "'%H:%M:%f'", "'%j'", "'%W'", "'%Z'"]
JP = ["'$.a'", "'$[0]'", "'$'", "'$.a.b'", "'$[#-1]'"]


def lit():
    return R.choice(LITS)


def expr(d):
    if d <= 0 or R.random() < 0.18:
        return lit()
    k = R.random()
    if k < 0.12:
        return R.choice(UN) + "(" + expr(d - 1) + ")"
    if k < 0.45:
        return "(" + expr(d - 1) + " " + R.choice(BIN) + " " + expr(d - 1) + ")"
    if k < 0.60:
        f = R.choice(F1)
        return call(f, [expr(d - 1)], d)
    if k < 0.70:
        f = R.choice(F2)
        return call(f, [expr(d - 1), expr(d - 1)], d)
    if k < 0.76:
        f = R.choice(F3)
        return call(f, [expr(d - 1), expr(d - 1), expr(d - 1)], d)
    if k < 0.82:
        return "CAST(" + expr(d - 1) + " AS " + R.choice(CASTS) + ")"
    if k < 0.86:
        return "(" + expr(d - 1) + " BETWEEN " + expr(d - 1) + " AND " + expr(d - 1) + ")"
    if k < 0.90:
        return "(" + expr(d - 1) + " IN (" + ", ".join(expr(d - 1) for _ in range(R.randint(1, 3))) + "))"
    if k < 0.94:
        return "CASE " + (expr(d - 1) + " " if R.random() < 0.5 else "") + "WHEN " + expr(d - 1) + " THEN " + expr(d - 1) + " ELSE " + expr(d - 1) + " END"
    if k < 0.97:
        return "(" + expr(d - 1) + " " + R.choice(["IS NULL", "NOTNULL", "ISNULL", "IS TRUE", "IS NOT TRUE", "IS FALSE", "IS NOT FALSE"]) + ")"
    return "(" + expr(d - 1) + " COLLATE " + R.choice(["NOCASE", "BINARY", "RTRIM"]) + " " + R.choice(["<", "=", ">"]) + " " + expr(d - 1) + ")"


def call(f, args, d):
    if f == "randomblob2":
        return "length(randomblob(" + str(R.randint(0, 4)) + "))"
    if f == "replace2":
        return "replace(" + ", ".join(args) + ", 'x')"
    if f == "iif2":
        return "iif(" + ", ".join(args) + ", 0)"
    if f == "nullif3":
        return "nullif(" + args[0] + ", " + args[1] + ")"
    if f == "printf" or f == "format":
        return f + "(" + R.choice(FMTS) + ", " + ", ".join(args[:R.randint(1, 2)]) + ")"
    if f == "strftime":
        return "strftime(" + R.choice(STRF) + ", " + ", ".join(args[:2] if R.random() < 0.5 else args[:1]) + ")"
    if f == "json_extract":
        return "json_extract(" + args[0] + ", " + R.choice(JP) + ")"
    if f in ("json_set", "json_insert"):
        return f + "(" + args[0] + ", " + R.choice(JP) + ", " + args[1] + ")"
    if f in ("substr", "round") and len(args) == 3:
        return f + "(" + ", ".join(args[:2 if f == "round" else 3]) + ")"
    if f in ("concat_ws", "char", "coalesce", "max", "min", "like", "glob", "date", "datetime", "iif", "concat", "trim", "ltrim", "rtrim", "instr", "nullif", "ifnull", "timediff", "unhex", "substr", "round", "replace"):
        if f in ("nullif", "ifnull", "instr", "like", "glob", "timediff", "unhex", "round"):
            args = args[:2]
        if f == "replace":
            args = args[:3]
        return f + "(" + ", ".join(args) + ")"
    return f + "(" + args[0] + ")"


for _ in range(n):
    print("SELECT " + expr(R.randint(1, 4)))
