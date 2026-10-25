"""probe.py a.sql [b.sql ...]: run each script through sqlrun and our build, print per-statement mismatches."""
import os, subprocess, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SQLRUN = os.path.join(ROOT, 'spec', 'b12', 'sqlrun')
CLJ = os.path.join(os.path.dirname(ROOT), 'toolchain', 'clj-run')
def chunks(t):
    p = t.split('~~~\n')
    return p[:-1] if p and p[-1] == '' else p
files = [os.path.abspath(f) for f in sys.argv[1:]]
refs = {f: subprocess.run([SQLRUN, f], capture_output=True, cwd=ROOT).stdout.decode('utf-8', 'surrogateescape') for f in files}
od = os.path.join(ROOT, 'scratch', 'out'); os.makedirs(od, exist_ok=True)
lst = os.path.join(ROOT, 'scratch', 'list.txt'); open(lst, 'w').write('\n'.join(files) + '\n')
subprocess.run([CLJ, '-m', 'sqlclj.main', '--batch', lst, '--out', od], capture_output=True, cwd=ROOT)
bad = tot = 0
for f in files:
    name = os.path.basename(f)[:-4]
    lines = [l for l in open(f, encoding='utf-8', errors='surrogateescape').read().split('\n') if l.strip()]
    want = chunks(refs[f])
    try: got = chunks(open(os.path.join(od, name + '.out'), encoding='utf-8', errors='surrogateescape').read())
    except FileNotFoundError: got = []
    for i, w in enumerate(want):
        tot += 1
        g = got[i] if i < len(got) else '<missing>\n'
        if g != w:
            bad += 1
            print('%s:%d: %s\n   want: %r\n   got:  %r' % (name, i + 1, lines[i][:300] if i < len(lines) else '?', w[:300], g[:300]))
print('mismatches: %d of %d' % (bad, tot))
