import glob, re, sys, os
root = os.path.join(os.getcwd(), 'spec/b12/corpus')
out = []
for f in sorted(glob.glob(root + '/*.sql')):
    lines = open(f, encoding='latin1').read().split('\n')
    ref = open(f[:-4] + '.ref', encoding='latin1').read().split('~~~\n')
    for i, l in enumerate(lines):
        if re.search(r'EXPLAIN', l, re.I) and i < len(ref):
            out.append((os.path.basename(f), i + 1, l, ref[i].rstrip('\n')))
for o in out:
    print('###', o[0], o[1])
    print(o[2][:600])
    print(o[3][:800])
