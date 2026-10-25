import subprocess, sys, os, re
# usage: ids.py "CREATE ...;..." "query1" "query2" ...   -> prints the ids SQLite assigns to each EXPLAIN QUERY PLAN
root = os.getcwd()
setup = sys.argv[1].split(';;')
qs = sys.argv[2:]
base = sys.argv[2:]
lines = setup + ['EXPLAIN QUERY PLAN ' + q for q in qs]
open(root + '/scratch/ids.sql', 'w').write('\n'.join(lines) + '\n')
real = subprocess.run([root + '/spec/b12/sqlrun', root + '/scratch/ids.sql'], capture_output=True).stdout.decode().split('~~~\n')
mine = subprocess.run(['sh', root + '/ptools/run.sh', root + '/scratch/ids.sql'], capture_output=True).stdout.decode().split('~~~\n')
for i, q in enumerate(qs):
    a = re.findall(r'(?m)^(\d+)\|\d+\|\d+\|(.*)$', real[len(setup) + i])
    b = re.findall(r'(?m)^(\d+)\|\d+\|\d+\|(.*)$', mine[len(setup) + i])
    print(q[:90].ljust(92), 'S', [int(x[0]) for x in a], 'M', [int(x[0]) for x in b], '' if [x[0] for x in a] == [x[0] for x in b] else '   <--')
