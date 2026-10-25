"""fzrun.py GEN FROM TO N : generate scratch/f<seed>.sql with tools/GEN.py and probe them all (differential vs sqlrun)."""
import os, subprocess, sys
ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
gen, a, b, n = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]
files = []
for s in range(a, b + 1):
    f = os.path.join(ROOT, 'scratch', 'f%d.sql' % s)
    with open(f, 'w') as fh:
        subprocess.run(['python3', '-I', os.path.join(ROOT, 'tools', gen + '.py'), str(s), n], stdout=fh, cwd=ROOT)
    files.append(f)
r = subprocess.run(['python3', '-I', os.path.join(ROOT, 'tools', 'probe.py')] + files, capture_output=True, cwd=ROOT)
sys.stdout.write(r.stdout.decode('utf-8', 'replace')[:20000])
