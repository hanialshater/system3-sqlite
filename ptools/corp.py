import subprocess, sys, os, glob
root = os.getcwd()
corpus = root + '/spec/b12/corpus'
tag = sys.argv[1]  # output tag
env = dict(os.environ)
if len(sys.argv) > 2 and sys.argv[2] == 'noplan':
    env['SQLCLJ_NOPLAN'] = '1'
lst = root + '/scratch/list_' + tag + '.txt'
files = sorted(glob.glob(corpus + '/*.sql'))
open(lst, 'w').write('\n'.join(files) + '\n')
out = root + '/scratch/out_' + tag
os.makedirs(out, exist_ok=True)
subprocess.run(['sh', root + '/scratch/run.sh', '--batch', lst, '--out', out], env=env, capture_output=True)
fails = []
for f in files:
    name = os.path.basename(f)[:-4]
    try:
        got = open(out + '/' + name + '.out', encoding='latin1').read().split('~~~\n')
    except FileNotFoundError:
        got = []
    ref = open(corpus + '/' + name + '.ref', encoding='latin1').read().split('~~~\n')
    lines = open(f, encoding='latin1').read().split('\n')
    lines = [l for l in lines if l]
    for i in range(len(ref) - 1):
        g = got[i] if i < len(got) else '<missing>'
        import re
        nz = lambda x: re.sub(r'(?m)^\d+\|(\d+)\|', r'X|\1|', x) if '# id|parent' in x else x
        if (nz(g) if 'noid' in sys.argv else g) != (nz(ref[i]) if 'noid' in sys.argv else ref[i]):
            fails.append('%s:%d' % (name, i + 1))
open(root + '/scratch/fails_' + tag + '.txt', 'w').write('\n'.join(fails) + '\n')
print(tag, len(fails))
