import sys, subprocess, os, re
root = os.getcwd()
f = sys.argv[1]
want = sys.argv[2:]  # substrings or indexes of queries
lines = [l for l in open(f, encoding='utf-8').read().split('\n') if l.strip()]
setup = [l for l in lines if not l.startswith('SELECT')]
qs = [l for l in lines if l.startswith('SELECT')]
sel = []
for w in want:
    if w.isdigit():
        sel.append(qs[int(w)])
    else:
        sel += [q for q in qs if w in q]
out = setup + ['EXPLAIN QUERY PLAN ' + q for q in sel] + sel
open(root + '/scratch/ex.sql', 'w').write('\n'.join(out) + '\n')
real = subprocess.run([root + '/spec/b12/sqlrun', root + '/scratch/ex.sql'], capture_output=True).stdout.decode().split('~~~\n')
mine = subprocess.run(['sh', root + '/scratch/run.sh', root + '/scratch/ex.sql'], capture_output=True).stdout.decode().split('~~~\n')
print('\n'.join(setup[-8:]))
n = len(setup)
for i, q in enumerate(sel):
    print('Q:', q)
    print('SQLITE EQP:', ' / '.join(x for x in real[n + i].split('\n')[1:] if x))
    print('MINE   EQP:', ' / '.join(x for x in mine[n + i].split('\n')[1:] if x))
    j = n + len(sel) + i
    print('SQLITE:', real[j].replace('\n', ' / ')[:250])
    print('MINE  :', mine[j].replace('\n', ' / ')[:250])
