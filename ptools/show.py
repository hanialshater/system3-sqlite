import sys, os
root = os.getcwd()
for a in sys.argv[1:]:
    name, n = a.rsplit(':', 1)
    n = int(n)
    ref = open(root + '/spec/b12/corpus/' + name + '.ref', encoding='latin1').read().split('~~~\n')
    got = open(root + '/scratch/out_plan/' + name + '.out', encoding='latin1').read().split('~~~\n')
    lines = [l for l in open(root + '/spec/b12/corpus/' + name + '.sql', encoding='latin1').read().split('\n') if l]
    print('##', a, lines[n - 1][:200])
    print('WANT:', ref[n - 1].replace('\n', ' / ')[:300])
    print('GOT :', (got[n - 1] if n - 1 < len(got) else '').replace('\n', ' / ')[:300])
