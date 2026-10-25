import random, subprocess, sys, os, re
root = os.getcwd()
seed = int(sys.argv[1]) if len(sys.argv) > 1 else 1
n = int(sys.argv[2]) if len(sys.argv) > 2 else 50
nq = int(sys.argv[3]) if len(sys.argv) > 3 else 12
random.seed(seed)
def schema():
    st = ['CREATE TABLE t(a, b INTEGER, c TEXT, d)']
    cols = ['a', 'b', 'c', 'd']
    for k in range(random.choice([0, 1, 2, 3])):
        cs = random.sample(cols, random.choice([1, 1, 2]))
        st.append('CREATE %sINDEX i%d ON t(%s)' % ('UNIQUE ' if random.random() < 0.1 else '', k, ', '.join(c + random.choice(['', '', ' DESC']) for c in cs)))
    if random.random() < 0.15:
        st.append('CREATE INDEX ip ON t(a) WHERE b>5')
    for i in range(random.choice([0, 6, 20])):
        st.append("INSERT INTO t VALUES(%d, %d, 'x%d', %s)" % (random.randint(1, 5), random.randint(1, 9), random.randint(1, 5), random.choice(['NULL', '1', "'q'"])))
    if random.random() < 0.3:
        st.append('ANALYZE')
    return st
def lit(c):
    if c == 'b': return str(random.randint(1, 9))
    if c == 'c': return "'x%d'" % random.randint(1, 5)
    return random.choice(['1', '2', "'q'", '3.5'])
def term():
    c = random.choice(['a', 'b', 'c', 'd'])
    r = random.random()
    if r < 0.35: return '%s=%s' % (c, lit(c))
    if r < 0.5: return '%s%s%s' % (c, random.choice(['>', '<', '>=', '<=']), lit(c))
    if r < 0.58: return '%s IN (%s,%s)' % (c, lit(c), lit(c))
    if r < 0.66: return '%s BETWEEN %s AND %s' % (c, lit(c), lit(c))
    if r < 0.72: return "%s LIKE 'x%%'" % c
    if r < 0.78: return '%s IS NULL' % c
    if r < 0.84: return "lower(%s)='x'" % c
    if r < 0.9: return '%s+1=%d' % (c, random.randint(1, 5))
    if r < 0.95: return '%s<>%s' % (c, lit(c))
    return '(%s=1 OR %s=2)' % (c, c)
def query():
    r = random.random()
    if r < 0.05:
        sel = 'count(*)'
    else:
        k = random.choice([1, 1, 2, 3, 4])
        cs = random.sample(['a', 'b', 'c', 'd'], k)
        sel = ', '.join(cs if random.random() < 0.85 else [c + '+1' if i == 0 else c for i, c in enumerate(cs)])
        if random.random() < 0.08: sel = '*'
    q = 'SELECT %s%s FROM t' % ('DISTINCT ' if random.random() < 0.1 else '', sel)
    nt = random.choice([0, 1, 1, 2, 3])
    if nt:
        q += ' WHERE ' + ' AND '.join(term() for _ in range(nt))
    if random.random() < 0.08:
        q += ' GROUP BY %s' % random.choice(['a', 'b', 'a, b', 'c'])
    elif random.random() < 0.3:
        ob = random.sample(['a', 'b', 'c', 'd'], random.choice([1, 1, 2]))
        q += ' ORDER BY ' + ', '.join(o + random.choice(['', '', ' DESC']) for o in ob)
    if random.random() < 0.1:
        q += ' LIMIT %d' % random.randint(1, 10)
    return q
def run(a):
    return subprocess.run(a, capture_output=True).stdout.decode('utf-8', 'replace')
os.makedirs(root + '/scratch/fzd', exist_ok=True)
cases = []
for it in range(n):
    st = schema()
    qs = ['EXPLAIN QUERY PLAN ' + query() for _ in range(nq)]
    f = root + '/scratch/fzd/q%d_%d.sql' % (seed, it)
    open(f, 'w').write('\n'.join(st + qs) + '\n')
    cases.append((it, st, qs, f))
open(root + '/scratch/fzd/list2.txt', 'w').write('\n'.join(c[3] for c in cases) + '\n')
run(['sh', root + '/scratch/run.sh', '--batch', root + '/scratch/fzd/list2.txt', '--out', root + '/scratch/fzd/out2'])
bad = textbad = 0
for it, st, qs, f in cases:
    real = run([root + '/spec/b12/sqlrun', f]).split('~~~\n')
    mine = open(root + '/scratch/fzd/out2/q%d_%d.out' % (seed, it), encoding='utf-8', errors='replace').read().split('~~~\n')
    for i, q in enumerate(qs):
        a = real[len(st) + i]; b = mine[len(st) + i] if len(st) + i < len(mine) else ''
        if a != b:
            bad += 1
            na = re.sub(r'(?m)^\d+\|(\d+)\|', r'X|\1|', a); nb = re.sub(r'(?m)^\d+\|(\d+)\|', r'X|\1|', b)
            tag = 'ID' if na == nb else 'TEXT'
            if tag == 'TEXT': textbad += 1
            if tag == 'ID':
                ia = re.findall(r'(?m)^(\d+)\|\d+\|', a); ib = re.findall(r'(?m)^(\d+)\|\d+\|', b)
                print('ID', q[19:], ' S', ia, ' M', ib, ' idx:', '|'.join(l[13:] for l in st if 'CREATE' in l and 'TABLE' not in l)[:80])
                continue
            print('===', tag, f.split('/')[-1], '|'.join(l for l in st if 'CREATE' in l)[:200])
            print(q)
            print(' S:', a.replace('\n', ' / ')[:300])
            print(' M:', b.replace('\n', ' / ')[:300])
print('bad', bad, 'textbad', textbad, 'of', n * nq)
