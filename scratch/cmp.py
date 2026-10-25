import subprocess, sys, os, re
root = os.getcwd()
f = sys.argv[1]
ignore_id = '--ids' not in sys.argv
real = subprocess.run([root + '/spec/b12/sqlrun', f], capture_output=True).stdout.decode('utf-8', 'replace')
mine = subprocess.run(['sh', root + '/scratch/run.sh', f], capture_output=True).stdout.decode('utf-8', 'replace')
lines = [l for l in open(f, encoding='utf-8').read().split('\n') if l.strip()]
rb = real.split('~~~\n')
mb = mine.split('~~~\n')
def norm(b):
    if not ignore_id:
        return b
    out = []
    for l in b.split('\n'):
        m = re.match(r'^(\d+)\|(\d+)\|(.*)$', l)
        if m:
            out.append('X|' + m.group(2) + '|' + m.group(3))
        else:
            out.append(l)
    return '\n'.join(out)
bad = 0
tot = 0
for i, l in enumerate(lines):
    if i >= len(rb):
        break
    if 'EXPLAIN' not in l.upper():
        continue
    tot += 1
    a = norm(rb[i]); b = norm(mb[i]) if i < len(mb) else ''
    if a != b:
        bad += 1
        print('--- line', i + 1, l[:200])
        print('SQLITE:', a.replace('\n', ' / '))
        print('MINE  :', b.replace('\n', ' / '))
print('mismatches', bad, 'of', tot)
