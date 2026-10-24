"""Collect SQLite query plans for the aggregates template in generated scripts.
Usage: python3 tools/plans.py <dir>"""
import collections
import os
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SQLRUN = os.path.join(ROOT, 'spec', 'b11', 'sqlrun')
d = sys.argv[1]
c = collections.Counter()
examples = {}
for f in sorted(os.listdir(d)):
    if not f.endswith('.sql'):
        continue
    lines = open(os.path.join(d, f), encoding='utf-8').read().split('\n')
    out = []
    for l in lines:
        if l.startswith('SELECT count(*), count('):
            out.append('EXPLAIN QUERY PLAN ' + l)
        else:
            out.append(l)
    tmp = os.path.join(d, 'plan.tmp')
    open(tmp, 'w').write('\n'.join(out))
    res = subprocess.run([SQLRUN, tmp], capture_output=True).stdout.decode('utf-8', 'replace')
    for chunk, l in zip(res.split('~~~\n'), [x for x in out if x.strip()]):
        if l.startswith('EXPLAIN'):
            plan = ' / '.join(x.split('|', 3)[-1] for x in chunk.strip().split('\n')[1:])
            key = plan.replace('sqlite_autoindex_', 'ai_')
            k2 = 'SCAN' if key.startswith('SCAN t') and 'INDEX' not in key else key[:60]
            c[k2] += 1
            examples.setdefault(k2, l)
for k, v in c.most_common():
    print(v, k, '   e.g.', examples[k][:150])
