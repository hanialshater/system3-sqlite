"""Extended random SQL generator (beyond the corpus templates). Usage: python3 tools/gen2.py <outdir> <count> <seed>"""
import os
import random
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gen as G
from gen import expr, rnd_value, rnd_text, rnd_int


def agg_fn(t):
    c = random.choice(['id'] + t.colnames())
    f = random.choice(['count', 'sum', 'total', 'avg', 'min', 'max', 'group_concat', 'count'])
    d = 'DISTINCT ' if random.random() < 0.2 else ''
    if f == 'group_concat' and random.random() < 0.4 and not d:
        return "group_concat(%s, %s)" % (c, rnd_text())
    if f == 'count' and random.random() < 0.3:
        return 'count(*)'
    return '%s(%s%s)' % (f, d, c)


def s_select(ts):
    t = random.choice(ts)
    r = random.random()
    cols = ', '.join(random.choice(['id'] + t.colnames() + [expr(t, 1)]) for _ in range(random.randint(1, 3)))
    w = (' WHERE ' + expr(t)) if random.random() < 0.6 else ''
    if r < 0.15:
        return 'SELECT %s FROM %s%s;' % (cols, t.name, w)  # no ORDER BY: scan order
    if r < 0.3:
        return 'SELECT DISTINCT %s FROM %s%s ORDER BY 1;' % (random.choice(t.colnames()), t.name, w)
    if r < 0.5:
        g = random.choice(t.colnames())
        h = random.choice(['', ' HAVING %s > %d' % (agg_fn(t), random.randint(0, 3)), ' HAVING ' + expr(t, 2)])
        return 'SELECT %s, %s, %s FROM %s%s GROUP BY %s%s ORDER BY 1;' % (g, agg_fn(t), agg_fn(t), t.name, w, g, h)
    if r < 0.6:
        return 'SELECT %s, %s FROM %s%s;' % (agg_fn(t), agg_fn(t), t.name, w)
    if r < 0.7:
        return 'SELECT %s FROM %s%s ORDER BY %s LIMIT %s OFFSET %s;' % (
            cols, t.name, w, random.choice(['1', '1 DESC', 'id DESC', random.choice(t.colnames())]),
            random.choice(['2', '1+1', '-1', '0', "'3'"]), random.choice(['0', '1', '2-1']))
    if r < 0.8:
        c = random.choice(t.colnames())
        return ("SELECT length(%s), upper(%s), lower(%s), substr(%s, %d), hex(%s), quote(%s), trim(%s), %s LIKE %s, "
                "%s GLOB %s, instr(%s, %s), replace(%s, %s, %s), unicode(%s) FROM %s ORDER BY id;") % (
            c, c, c, c, random.randint(-3, 4), c, c, c, c, random.choice(["'%a%'", "'_%'", "'A%'", rnd_text()]), c,
            random.choice(["'*a*'", "'[a-z]*'", "'?*'"]), c, rnd_text(), c, rnd_text(), rnd_text(), c, t.name)
    if r < 0.9:
        c = random.choice(t.colnames())
        return "SELECT id, %s LIKE %s ESCAPE '\\', CAST(%s AS %s), %s COLLATE NOCASE < %s FROM %s ORDER BY id;" % (
            c, random.choice(["'a\\%%'", "'%\\_%'", rnd_text()]), c,
            random.choice(['INT', 'TEXT', 'REAL', 'NUMERIC', 'BLOB', 'VARCHAR(5)', 'FLOAT', 'BOOLEAN']),
            c, rnd_text(), t.name)
    return 'SELECT %s FROM %s ORDER BY %s;' % (', '.join('%s IS %s' % (random.choice(t.colnames()), rnd_value()) for _ in range(2)), t.name, 'id')


def s_join(ts):
    a, b = random.choice(ts), random.choice(ts)
    r = random.random()
    if r < 0.3 and len(ts) >= 3:
        c = ts[2]
        return ('SELECT a.id, b.id, c.id FROM %s a LEFT JOIN %s b ON a.id = b.id + 1 JOIN %s c ON c.id >= a.id '
                'WHERE %s ORDER BY 1, 2, 3 LIMIT 30;') % (a.name, b.name, c.name, random.choice(['1', 'b.id IS NULL', 'a.id > 2', 'c.id < 4']))
    if r < 0.5:
        return 'SELECT * FROM %s NATURAL JOIN %s ORDER BY 1 LIMIT 20;' % (a.name, b.name)
    if r < 0.7:
        common = [c for c in a.colnames() if c in b.colnames()] or ['id']
        u = ', '.join(random.sample(common, min(len(common), random.randint(1, 2))))
        return 'SELECT * FROM %s JOIN %s USING(%s) ORDER BY 1 LIMIT 20;' % (a.name, b.name, u)
    if r < 0.85:
        return 'SELECT a.id, count(b.id), max(b.%s) FROM %s a LEFT JOIN %s b ON b.id < a.id GROUP BY a.id ORDER BY 1;' % (
            random.choice(b.colnames()), a.name, b.name)
    return 'SELECT a.id, b.id FROM %s a, %s b WHERE a.%s = b.%s ORDER BY 1, 2;' % (
        a.name, b.name, random.choice(a.colnames()), random.choice(b.colnames()))


def s_sub(ts):
    a, b = random.choice(ts), random.choice(ts)
    r = random.random()
    if r < 0.2:
        return 'SELECT id, (SELECT max(%s) FROM %s WHERE %s.id < %s.id), (SELECT %s FROM %s ORDER BY id DESC LIMIT 1) FROM %s ORDER BY id;' % (
            random.choice(b.colnames()), b.name, b.name, a.name, random.choice(b.colnames()), b.name, a.name)
    if r < 0.4:
        return 'SELECT id FROM %s WHERE %s IN (SELECT %s FROM %s) ORDER BY id;' % (
            a.name, random.choice(a.colnames()), random.choice(b.colnames()), b.name)
    if r < 0.55:
        ops = [random.choice(['UNION', 'UNION ALL', 'INTERSECT', 'EXCEPT']) for _ in range(2)]
        return 'SELECT %s FROM %s %s SELECT %s FROM %s %s SELECT %s FROM %s%s;' % (
            random.choice(a.colnames()), a.name, ops[0], random.choice(b.colnames()), b.name, ops[1],
            random.choice(a.colnames()), a.name, random.choice(['', ' ORDER BY 1', ' ORDER BY 1 DESC LIMIT 5']))
    if r < 0.7:
        return 'SELECT * FROM (SELECT id, %s AS v FROM %s WHERE %s) AS s WHERE v IS NOT NULL ORDER BY id;' % (
            expr(a, 1), a.name, expr(a, 1))
    if r < 0.85:
        return 'SELECT %s, count(*) FROM %s GROUP BY 1 HAVING count(*) >= (SELECT count(*) FROM %s) / 4 ORDER BY 2 DESC, 1;' % (
            random.choice(a.colnames()), a.name, b.name)
    return 'SELECT id FROM %s x WHERE %s > (SELECT avg(id) FROM %s y WHERE y.id <> x.id) ORDER BY id;' % (
        a.name, random.choice(['id', 'id + 0']), b.name)


def s_dml(ts):
    t = random.choice(ts)
    c = random.choice(t.colnames())
    r = random.random()
    if r < 0.12:
        return 'INSERT INTO %s(%s) SELECT %s FROM %s WHERE id %% 2 = 0;' % (t.name, c, random.choice(['id', c, 'id * 10']), t.name)
    if r < 0.24:
        return 'INSERT INTO %s(%s) VALUES(%s), (%s), (%s);' % (t.name, c, rnd_value(), rnd_value(), rnd_value())
    if r < 0.36:
        return 'UPDATE OR %s %s SET %s = %s WHERE %s;' % (random.choice(['IGNORE', 'REPLACE']), t.name, c, rnd_value(), expr(t, 1))
    if r < 0.46:
        return 'DELETE FROM %s WHERE id IN (SELECT id FROM %s WHERE %s) RETURNING *;' % (t.name, t.name, expr(t, 1))
    if r < 0.56:
        return 'INSERT INTO %s(id, %s) VALUES(%d, %s) ON CONFLICT DO NOTHING;' % (t.name, c, random.randint(1, 9), rnd_value())
    if r < 0.66:
        uc = [x[0] for x in t.cols if 'UNIQUE' in x[2]]
        if uc:
            u = uc[0]
            return 'INSERT INTO %s(%s) VALUES(%s) ON CONFLICT(%s) DO UPDATE SET %s = excluded.%s WHERE %s RETURNING id;' % (
                t.name, u, rnd_value(), u, u, u, expr(t, 2))
        return 'INSERT INTO %s(id) VALUES(%d) ON CONFLICT(id) DO UPDATE SET id = id + 100;' % (t.name, random.randint(1, 9))
    if r < 0.76:
        return 'UPDATE %s SET %s = %s, id = id + %d WHERE %s RETURNING id, %s;' % (
            t.name, c, expr(t, 1), random.randint(0, 3), expr(t, 2), c)
    if r < 0.86:
        return 'REPLACE INTO %s VALUES(%s);' % (t.name, ', '.join([str(random.randint(1, 9))] + [rnd_value() for _ in t.colnames()]))
    return 'INSERT INTO %s DEFAULT VALUES;' % t.name


def s_index(ts):
    t = random.choice(ts)
    c = random.choice(t.colnames())
    n = random.randint(1, 99)
    r = random.random()
    if r < 0.4:
        return ['CREATE %sINDEX i%d ON %s(%s)%s;' % (random.choice(['', 'UNIQUE ']), n, t.name,
                                                   random.choice([c, c + ' COLLATE NOCASE', 'lower(%s)' % c, '%s, id DESC' % c]),
                                                   random.choice(['', ' WHERE %s > 0' % c])),
                'SELECT id, %s FROM %s WHERE %s > %s;' % (c, t.name, c, rnd_value()),
                'SELECT %s FROM %s INDEXED BY i%d WHERE %s;' % (c, t.name, n, random.choice(['%s > 0' % c, '%s IS NOT NULL' % c, '1'])),
                'DROP INDEX i%d;' % n]
    if r < 0.6:
        return ['DROP INDEX i%d;' % n, 'REINDEX;', 'REINDEX nocase;', 'DROP INDEX IF EXISTS sqlite_autoindex_%s_1;' % t.name]
    return ['CREATE INDEX i%d ON %s(%s);' % (n, t.name, c), 'CREATE INDEX i%d ON %s(%s);' % (n, t.name, c),
            'SELECT count(*) FROM %s NOT INDEXED;' % t.name, 'DROP TABLE IF EXISTS tx;', 'DROP INDEX i%d;' % n]


def gen_script():
    ntab = 3
    tables = []
    lines = []
    for i in range(ntab):
        t = G.gen_table('t%d' % i, random.randint(2, 4), tables[-1].name if tables else None)
        tables.append(t)
        lines.append(G.table_sql(t))
        for _ in range(random.randint(2, 10)):
            lines.append(G.insert_sql(t))
    for _ in range(random.randint(20, 40)):
        f = random.choice([s_select, s_select, s_join, s_sub, s_dml, s_dml, s_index])
        x = f(tables)
        lines.extend(x if isinstance(x, list) else [x])
    return '\n'.join(lines) + '\n'


def main():
    out, count, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    random.seed(seed)
    os.makedirs(out, exist_ok=True)
    for i in range(count):
        with open(os.path.join(out, 'ext-%04d.sql' % i), 'w') as fh:
            fh.write(gen_script())


if __name__ == '__main__':
    main()
