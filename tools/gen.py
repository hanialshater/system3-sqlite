"""Random SQL script generator in the style of the B11 corpus (for differential testing against sqlrun).
Usage: python3 tools/gen.py <outdir> <count> <seed> [family]"""
import os
import random
import sys

WORDS = ['alpha', 'beta', 'gamma', 'delta', 'epsilon', 'zeta', 'eta', 'theta', 'iota', 'kappa', 'Lambda', 'MU',
         'nu', 'xi', 'omicron', 'pi', 'rho', 'sigma', 'tau', 'upsilon', 'phi', 'chi', 'psi', 'omega', 'a%b', 'x_y',
         "it''s", ' pad ', '', 'NULL', '42', '-7', '1e3', '0x1F', 'café', 'naïve', 'señor', 'über', '日本',
         'Ελλάδα', 'A', 'abc', 'ABC', 'Abc', '3.0', ' 12 ', '-0', '1.5e2', 'x']
SPECIAL_NUM = ['0', '1', '-1', '2', '3', '4', '5', '-2', '-3', '-4', '-5', '42', '123456789012',
               '9223372036854775807', '-9223372036854775808', '2.5', '-2.5', '0.1', '1e308', '-1e308',
               '4.5e-310', '1.0e15', '1e-7', '3.141592653589793', '1e3', '0.0', '-0.0', '100', '1000', '-1000',
               '9223372036854775806', '-9223372036854775807', '0x10', '0xff', '1.5', '7.25']
TYPES = ['INTEGER', 'INT', 'TEXT', 'REAL', 'NUMERIC', 'BLOB', 'VARCHAR(10)', '']
STRICT_TYPES = ['INTEGER', 'INT', 'TEXT', 'REAL', 'BLOB', 'ANY']


def rnd_int():
    r = random.random()
    if r < 0.5:
        return str(random.randint(-10, 10))
    if r < 0.8:
        return str(random.randint(-1000, 1000))
    return random.choice(SPECIAL_NUM)


def rnd_real():
    r = random.random()
    if r < 0.6:
        d = random.randint(1, 4)
        return ('%.' + str(d) + 'f') % random.uniform(-1000, 1000)
    return random.choice(['2.5', '-2.5', '0.1', '1e308', '-1e308', '4.5e-310', '1.0e15', '1e-7',
                          '3.141592653589793', '0.0', '1.5', '-0.5', '123456789012.0', '1e3', '2.0'])


def rnd_text():
    return "'" + random.choice(WORDS) + "'"


def rnd_blob():
    n = random.choice([0, 1, 1, 2, 3, 4, 5, 6])
    return "x'" + ''.join(random.choice('0123456789abcdef') for _ in range(2 * n)) + "'"


def rnd_value():
    r = random.random()
    if r < 0.12:
        return 'NULL'
    if r < 0.45:
        return rnd_int()
    if r < 0.6:
        return rnd_real()
    if r < 0.85:
        return rnd_text()
    return rnd_blob()


class Table:
    def __init__(self, name, cols, strict):
        self.name = name
        self.cols = cols  # list of (name, type, constraints)
        self.strict = strict

    def colnames(self):
        return [c[0] for c in self.cols]


def gen_table(name, ncols, prev=None):
    strict = random.random() < 0.15
    cols = []
    for i in range(ncols):
        t = random.choice(STRICT_TYPES if strict else TYPES)
        cons = []
        r = random.random()
        if r < 0.12:
            cons.append('NOT NULL DEFAULT 0')
        elif r < 0.22:
            cons.append("DEFAULT 'd'")
        elif r < 0.32:
            cons.append('UNIQUE')
        elif r < 0.45:
            cons.append('CHECK(c%d IS NULL OR length(c%d) < 40)' % (i, i))
        if random.random() < 0.15 and not strict:
            cons.append('COLLATE NOCASE')
        cols.append(('c%d' % i, t, cons))
    if prev is not None and random.random() < 0.6:
        cols.append(('ref', 'INTEGER', ['REFERENCES %s(id)' % prev]))
    return Table(name, cols, strict)


def table_sql(t):
    parts = ['id INTEGER PRIMARY KEY']
    for (n, ty, cons) in t.cols:
        parts.append(' '.join([n, ty] + cons) if ty or cons else n)
    return 'CREATE TABLE %s(%s)%s;' % (t.name, ', '.join(parts).replace('  ', ' '), ' STRICT' if t.strict else '')


def insert_sql(t):
    cols = t.colnames()
    k = random.randint(1, len(cols))
    use = cols if random.random() < 0.7 else cols[:k]
    vals = []
    for c in use:
        if c == 'ref':
            vals.append(random.choice(['NULL', str(random.randint(1, 8))]))
        else:
            vals.append(rnd_value())
    verb = 'INSERT OR IGNORE INTO' if random.random() < 0.3 else 'INSERT INTO'
    return '%s %s(%s) VALUES(%s);' % (verb, t.name, ', '.join(use), ', '.join(vals))


def col_ref(t, alias=None):
    c = random.choice(['id'] + t.colnames())
    return (alias + '.' + c) if alias else c


def expr(t, depth=0, alias=None, allow_col=True):
    if depth > 2 or random.random() < 0.3:
        if allow_col and t is not None and random.random() < 0.5:
            return col_ref(t, alias)
        return rnd_value()
    r = random.randint(0, 22)
    e = lambda: expr(t, depth + 1, alias, allow_col)
    if r == 0:
        return '-(%s)' % e()
    if r == 1:
        return 'NOT %s' % e()
    if r == 2:
        return 'abs(%s)' % e()
    if r == 3:
        return 'typeof(%s)' % e()
    if r == 4:
        return 'coalesce(%s, %s)' % (e(), e())
    if r == 5:
        return 'nullif(%s, %s)' % (e(), e())
    if r == 6:
        return 'iif(%s, %s, %s)' % (e(), e(), e())
    if r == 7:
        return 'CASE WHEN %s THEN %s ELSE %s END' % (e(), e(), e())
    if r == 8:
        return 'CAST(%s AS %s)' % (e(), random.choice(['INTEGER', 'TEXT', 'REAL', 'NUMERIC', 'BLOB']))
    if r == 9:
        return '%s BETWEEN %s AND %s' % (e(), e(), e())
    if r == 10:
        return '%s IN (%s, %s, %s)' % (e(), e(), e(), e())
    if r == 11:
        return '%s IS NULL' % e()
    if r == 12:
        return '%s IS NOT NULL' % e()
    if r == 13:
        return '(%s %s %s)' % (e(), random.choice(['+', '-', '*', '/', '%']), e())
    if r == 14:
        return '(%s %s %s)' % (e(), random.choice(['||', '&', '|', '<<', '>>']), e())
    if r == 15:
        return '(%s %s %s)' % (e(), random.choice(['=', '<>', '<', '<=', '>', '>=', '==', '!=']), e())
    if r == 16:
        return '(%s %s %s)' % (e(), random.choice(['AND', 'OR']), e())
    if r == 17:
        return '%s IS %s' % (e(), e())
    if r == 18:
        return '%s IS NOT %s' % (e(), e())
    if r == 19:
        return '%s NOT IN (%s, %s)' % (e(), e(), e())
    if r == 20:
        return '%s NOT BETWEEN %s AND %s' % (e(), e(), e())
    if r == 21:
        return '~%s' % e()
    return '%s COLLATE NOCASE = %s' % (e(), e())


def order_cols(t):
    cols = ['id'] + t.colnames()
    random.shuffle(cols)
    first = random.choice(['id', 'id DESC'] + [c for c in t.colnames()])
    rest = [c + random.choice(['', '', ' DESC']) for c in t.colnames()]
    return ', '.join([first] + rest + (['id'] if not first.startswith('id') else []))


def stmt_aggregate(ts):
    t = random.choice(ts)
    c = random.choice(['id'] + t.colnames())
    r = random.random()
    if r < 0.45:
        w = (' WHERE ' + expr(t)) if random.random() < 0.5 else ''
        return 'SELECT count(*), count(%s), sum(%s), total(%s), avg(%s), min(%s), max(%s), count(DISTINCT %s) FROM %s%s;' % (
            c, c, c, c, c, c, c, t.name, w)
    if r < 0.85:
        sep = random.choice(['', ", ';'"])
        hv = random.choice(['', '', ' HAVING count(*) > %d' % random.randint(0, 2)])
        return 'SELECT %s, count(*), group_concat(id%s) FROM (SELECT * FROM %s ORDER BY id) GROUP BY %s%s ORDER BY %s;' % (
            c, sep, t.name, c, hv, c)
    return 'SELECT %s, group_concat(%s), max(%s), min(%s), %s FROM %s GROUP BY %s;' % (
        c, random.choice(t.colnames()), c, c, random.choice(t.colnames()), t.name, random.choice(t.colnames()))


def stmt_dml(ts):
    t = random.choice(ts)
    r = random.random()
    c = random.choice(t.colnames())
    if r < 0.2:
        w = (' WHERE ' + expr(t)) if random.random() < 0.7 else ''
        return 'UPDATE %s SET %s = %s%s;' % (t.name, c, expr(t), w)
    if r < 0.35:
        return 'DELETE FROM %s WHERE id %% %d = %d;' % (t.name, random.randint(2, 5), random.randint(0, 2))
    if r < 0.5:
        return 'REPLACE INTO %s(id, %s) VALUES(%d, %s);' % (t.name, c, random.randint(1, 9), rnd_value())
    if r < 0.65:
        return 'SELECT * FROM %s ORDER BY %s;' % (t.name, order_cols(t))
    if r < 0.8:
        return 'INSERT INTO %s(id, %s) VALUES(%d, %s) ON CONFLICT(id) DO UPDATE SET %s = excluded.%s || \'!\';' % (
            t.name, c, random.randint(1, 9), rnd_value(), c, c)
    if r < 0.9:
        return 'INSERT INTO %s(%s) VALUES(%s) RETURNING id, %s;' % (t.name, c, rnd_value(), c)
    return 'INSERT OR %s INTO %s(%s) VALUES(%s);' % (random.choice(['IGNORE', 'REPLACE']), t.name, c, rnd_value())


def stmt_expr(ts):
    t = random.choice(ts)
    r = random.random()
    if r < 0.5:
        return 'SELECT id, %s, %s FROM %s ORDER BY %s;' % (expr(t), expr(t), t.name, order_cols(t))
    return 'SELECT %s, %s;' % (expr(None), expr(None))


def stmt_strings(ts):
    s = rnd_text()
    o = rnd_text()
    return ("SELECT length(%s), upper(%s), lower(%s), substr(%s, %d, %d), instr(%s, %s), replace(%s, %s, %s), "
            "trim(%s), ltrim(%s, 'ab'), hex(%s), quote(%s), unicode(%s), %s LIKE %s, %s GLOB %s, char(%d, %d), "
            "%s || %s, %s < %s, %s = %s COLLATE NOCASE;") % (
        s, s, s, s, random.randint(-3, 5), random.randint(-2, 5), s, o, s, random.choice(["'a'", "''", "'é'", o]),
        random.choice(["'XY'", "''"]), s, s, s, s, s, s, random.choice(["'a%'", "'%a_'", "'A%'", "'_'", "'%'", o]),
        s, random.choice(["'*'", "'?'", "'*[a-m]*'", "'[^a]*'", o]), random.randint(65, 90),
        random.randint(100, 1000), s, o, s, o, s, o)


def stmt_index(ts, state):
    t = random.choice(ts)
    c = random.choice(['id'] + t.colnames())
    n = random.randint(1, 99)
    uniq = 'UNIQUE ' if random.random() < 0.25 else ''
    where = (' WHERE %s IS NOT NULL' % c) if random.random() < 0.3 else ''
    desc = ' DESC' if random.random() < 0.3 else ''
    op = random.choice(['=', '>', '<', '>=', '<=', 'IS NOT', 'IS'])
    return ['CREATE %sINDEX IF NOT EXISTS i%d ON %s(%s%s, id)%s;' % (uniq, n, t.name, c, desc, where),
            'SELECT id, %s FROM %s WHERE %s %s %s ORDER BY %s, id;' % (c, t.name, c, op, rnd_value(), c),
            'SELECT count(*) FROM %s INDEXED BY i%d WHERE %s IS NOT NULL;' % (t.name, n, c),
            'REINDEX %s;' % t.name,
            'DROP INDEX IF EXISTS i%d;' % n]


def stmt_join(ts):
    a = random.choice(ts)
    b = random.choice(ts)
    r = random.random()
    if r < 0.6:
        jt = random.choice(['JOIN', 'LEFT JOIN', 'LEFT OUTER JOIN', 'CROSS JOIN'])
        on = random.choice(['a.id = b.id', 'a.id < b.id', 'a.id >= b.id', 'b.ref = a.id' if 'ref' in b.colnames() else 'a.id = b.id'])
        return 'SELECT a.id, b.id, a.%s, b.%s FROM %s a %s %s b ON %s ORDER BY 1, 2 LIMIT %d;' % (
            random.choice(['id'] + a.colnames()), random.choice(['id'] + b.colnames()), a.name, jt, b.name, on,
            random.randint(5, 60))
    w = ''
    rr = random.random()
    if rr < 0.4:
        w = ' WHERE ' + expr(a)
    elif rr < 0.6:
        w = ' WHERE a.' + random.choice(a.colnames())
    elif rr < 0.7:
        w = ' WHERE a.' + expr(a)
    return 'SELECT count(*) FROM %s a JOIN %s b USING(id)%s;' % (a.name, a.name, w)


def stmt_subquery(ts):
    a = random.choice(ts)
    b = random.choice(ts)
    r = random.random()
    if r < 0.25:
        return 'SELECT id, (SELECT count(*) FROM %s WHERE %s.id <= %s.id) FROM %s ORDER BY %s, id%s;' % (
            b.name, b.name, a.name, a.name, random.choice(['id'] + a.colnames()), random.choice(['', ' DESC']))
    if r < 0.45:
        w = (' WHERE ' + expr(b)) if random.random() < 0.5 else ''
        return 'SELECT id FROM %s WHERE id %sIN (SELECT id FROM %s%s) ORDER BY id;' % (
            a.name, random.choice(['', 'NOT ']), b.name, w)
    if r < 0.65:
        return 'SELECT id FROM %s x WHERE %sEXISTS (SELECT 1 FROM %s y WHERE y.id = x.id + %d) ORDER BY id;' % (
            a.name, random.choice(['', 'NOT ']), b.name, random.randint(0, 3))
    op = random.choice(['UNION', 'UNION ALL', 'INTERSECT', 'EXCEPT'])
    return 'SELECT %s FROM %s %s SELECT %s FROM %s ORDER BY 1 LIMIT %d OFFSET %d;' % (
        random.choice(['id'] + a.colnames()), a.name, op, random.choice(['id'] + b.colnames()), b.name,
        random.randint(1, 30), random.randint(0, 3))


FAMILIES = ['aggregates', 'dml', 'expr', 'strings', 'indexes', 'joins', 'subqueries']


def gen_script(family):
    ntab = random.randint(2, 3)
    tables = []
    lines = []
    for i in range(ntab):
        t = gen_table('t%d' % i, random.randint(2, 5), tables[-1].name if tables else None)
        tables.append(t)
        lines.append(table_sql(t))
        for _ in range(random.randint(2, 12)):
            lines.append(insert_sql(t))
    body = []
    state = {}
    for _ in range(random.randint(15, 35)):
        r = random.random()
        fam = family if r < 0.6 else random.choice(FAMILIES)
        if fam == 'aggregates':
            body.append(stmt_aggregate(tables))
        elif fam == 'dml':
            body.append(stmt_dml(tables))
        elif fam == 'expr':
            body.append(stmt_expr(tables))
        elif fam == 'strings':
            body.append(stmt_strings(tables))
        elif fam == 'indexes':
            body.extend(stmt_index(tables, state))
        elif fam == 'joins':
            body.append(stmt_join(tables))
        else:
            body.append(stmt_subquery(tables))
    return '\n'.join(lines + body) + '\n'


def main():
    out, count, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    fam = sys.argv[4] if len(sys.argv) > 4 else None
    random.seed(seed)
    os.makedirs(out, exist_ok=True)
    for i in range(count):
        f = fam or FAMILIES[i % len(FAMILIES)]
        with open(os.path.join(out, '%s-%04d.sql' % (f, i)), 'w') as fh:
            fh.write(gen_script(f))


if __name__ == '__main__':
    main()
