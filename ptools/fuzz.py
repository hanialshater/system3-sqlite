import random, subprocess, sys, os
root = os.getcwd()
seed = int(sys.argv[1]) if len(sys.argv) > 1 else 1
n = int(sys.argv[2]) if len(sys.argv) > 2 else 60
mode = sys.argv[3] if len(sys.argv) > 3 else 'rows'   # rows | eqp
random.seed(seed)
types = ['', 'INTEGER', 'TEXT', 'REAL', 'NUMERIC', 'TEXT COLLATE NOCASE']
def gen_schema():
    stmts = []
    ntab = random.choice([1, 2, 2, 3])
    tabs = []
    for t in range(ntab):
        ncol = random.choice([2, 3, 4])
        cols = []
        for c in range(ncol):
            ty = random.choice(types)
            cols.append(('c%d' % c, ty))
        ipk = random.random() < 0.3
        defs = []
        for i, (c, ty) in enumerate(cols):
            if i == 0 and ipk:
                defs.append('%s INTEGER PRIMARY KEY' % c)
            elif random.random() < 0.1:
                defs.append('%s %s UNIQUE' % (c, ty))
            else:
                defs.append('%s %s' % (c, ty))
        name = 't%d' % (t + 1)
        stmts.append('CREATE TABLE %s(%s)' % (name, ', '.join(defs)))
        nrows = random.choice([0, 3, 8, 15])
        vals = [0, 1, 2, 3, 4, 5, 'a', 'b', 'B', 'c', None, 2.5]
        for r in range(nrows):
            row = []
            for i, (c, ty) in enumerate(cols):
                if i == 0 and ipk:
                    row.append(str(r + 1 + random.choice([0, 0, 3])))
                else:
                    v = random.choice(vals)
                    row.append('NULL' if v is None else ("'%s'" % v if isinstance(v, str) else str(v)))
            stmts.append('INSERT OR IGNORE INTO %s VALUES(%s)' % (name, ', '.join(row)))
        for k in range(random.choice([0, 1, 2, 3])):
            cs = random.sample([c for c, _ in cols], random.choice([1, 1, 2]))
            ds = ['%s%s' % (c, random.choice(['', '', ' DESC', ' COLLATE NOCASE'])) for c in cs]
            stmts.append('CREATE %sINDEX %s_i%d ON %s(%s)' % ('UNIQUE ' if random.random() < 0.1 else '', name, k, name, ', '.join(ds)))
        tabs.append((name, [c for c, _ in cols]))
    if random.random() < 0.5:
        stmts.append('ANALYZE')
    return stmts, tabs
def lit():
    v = random.choice([0, 1, 2, 3, 5, "'a'", "'B'", "'c'", 2.5, 'NULL'])
    return str(v)
def gen_query(tabs):
    k = random.choice([1, 1, 2, 2, 3]) if len(tabs) > 1 else 1
    chosen = random.sample(tabs, min(k, len(tabs))) if k <= len(tabs) else tabs
    aliases = ['x%d' % i for i in range(len(chosen))]
    froms = []
    use_left = len(chosen) > 1 and random.random() < 0.15
    conds = []
    for i, ((tn, cols), al) in enumerate(zip(chosen, aliases)):
        if i == 0:
            froms.append('%s AS %s' % (tn, al))
        else:
            if use_left and i == len(chosen) - 1:
                froms.append('LEFT JOIN %s AS %s ON %s.%s=%s.%s' % (tn, al, al, random.choice(cols), aliases[random.randrange(i)], random.choice(chosen[random.randrange(i)][1]) if False else random.choice(chosen[0][1])))
            else:
                froms.append(', %s AS %s' % (tn, al) if random.random() < 0.7 else 'JOIN %s AS %s' % (tn, al))
    # join conditions
    for i in range(1, len(chosen)):
        if random.random() < 0.8:
            j = random.randrange(i)
            conds.append('%s.%s=%s.%s' % (aliases[i], random.choice(chosen[i][1]), aliases[j], random.choice(chosen[j][1])))
    for i, (tn, cols) in enumerate(chosen):
        r = random.random()
        c = random.choice(cols)
        if r < 0.25:
            conds.append('%s.%s=%s' % (aliases[i], c, lit()))
        elif r < 0.35:
            conds.append('%s.%s>%s' % (aliases[i], c, lit()))
        elif r < 0.42:
            conds.append('%s.%s IN (%s,%s,%s)' % (aliases[i], c, lit(), lit(), lit()))
        elif r < 0.48:
            conds.append('%s.%s BETWEEN %s AND %s' % (aliases[i], c, lit(), lit()))
        elif r < 0.52:
            conds.append('%s.%s IS NULL' % (aliases[i], c))
    sel = ', '.join('%s.%s' % (a, c) for a, (tn, cols) in zip(aliases, chosen) for c in cols[:2])
    q = 'SELECT %s%s FROM %s' % ('DISTINCT ' if random.random() < 0.1 else '', sel, ' '.join(froms).replace(' ,', ','))
    if conds and 'LEFT' not in q or conds:
        q += ' WHERE ' + ' AND '.join(conds) if conds and not (use_left) else ''
    if random.random() < 0.3:
        al = random.choice(aliases)
        cols = chosen[aliases.index(al)][1]
        q += ' ORDER BY %s.%s%s' % (al, random.choice(cols), random.choice(['', ' DESC']))
    return q
def run(args):
    return subprocess.run(args, capture_output=True).stdout.decode('utf-8', 'replace')
bad = 0
os.makedirs(root + '/scratch/fzd', exist_ok=True)
cases = []
import re
norm = lambda x: re.sub(r'(?m)^\d+\|(\d+)\|', r'X|\1|', x)
for it in range(n):
    stmts, tabs = gen_schema()
    qs0 = [gen_query(tabs) for _ in range(8)]
    qs = []
    for q in qs0:
        qs.append(q)
        qs.append('EXPLAIN QUERY PLAN ' + q)
    script = '\n'.join(stmts + qs) + '\n'
    f = root + '/scratch/fzd/s%d_%d.sql' % (seed, it)
    open(f, 'w').write(script)
    cases.append((it, stmts, qs, f, script))
open(root + '/scratch/fzd/list.txt', 'w').write('\n'.join(c[3] for c in cases) + '\n')
run(['sh', root + '/scratch/run.sh', '--batch', root + '/scratch/fzd/list.txt', '--out', root + '/scratch/fzd/out'])
nrow = neqp = 0
for it, stmts, qs, f, script in cases:
    real = run([root + '/spec/b12/sqlrun', f]).split('~~~\n')
    mine = open(root + '/scratch/fzd/out/s%d_%d.out' % (seed, it), encoding='utf-8', errors='replace').read().split('~~~\n')
    off = len(stmts)
    for i in range(0, len(qs), 2):
        q = qs[i]
        a = real[off + i] if off + i < len(real) else ''
        b = mine[off + i] if off + i < len(mine) else ''
        ea = norm(real[off + i + 1]) if off + i + 1 < len(real) else ''
        eb = norm(mine[off + i + 1]) if off + i + 1 < len(mine) else ''
        rowbad = a != b
        eqpbad = ea != eb
        if mode == 'rows' and not rowbad:
            continue
        if mode == 'eqp' and not eqpbad:
            continue
        sa = sorted(a.split('\n')); sb = sorted(b.split('\n'))
        kind = ('ORDER' if sa == sb else 'VALUE') if rowbad else 'EQPONLY'
        bad += 1
        print('=== seed', seed, 'it', it, kind, f)
        print('\n'.join(l for l in stmts if l.startswith('CREATE'))[:600])
        print(q)
        print('SQLITE:', a.replace('\n', ' / ')[:250])
        print('MINE  :', b.replace('\n', ' / ')[:250])
        print('EQP SQLITE:', ea.replace('\n', ' / ')[:300])
        print('EQP MINE  :', eb.replace('\n', ' / ')[:300])
print('bad', bad)
