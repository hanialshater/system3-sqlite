"""Differential test: runs sqlrun (reference) and our implementation (batch) on every .sql in a dir and
reports statement-level mismatches. Usage: python3 tools/diff.py <dir> [maxshow]"""
import os
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SQLRUN = os.path.join(ROOT, 'spec', 'b11', 'sqlrun')
RUN = os.path.join(ROOT, 'tools', 'run.sh')


def split_out(text):
    parts = text.split('~~~\n')
    return parts[:-1] if parts and parts[-1] == '' else parts


def main():
    d = sys.argv[1]
    maxshow = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    files = sorted(f for f in os.listdir(d) if f.endswith('.sql'))
    for f in files:
        ref = os.path.join(d, f[:-4] + '.ref')
        if not os.path.exists(ref):
            with open(ref, 'wb') as fh:
                fh.write(subprocess.run([SQLRUN, os.path.join(d, f)], capture_output=True).stdout)
    lst = os.path.join(d, 'list.txt')
    with open(lst, 'w') as fh:
        fh.write('\n'.join(os.path.join(d, f) for f in files) + '\n')
    outdir = os.path.join(d, 'out')
    os.makedirs(outdir, exist_ok=True)
    subprocess.run([RUN, '--batch', lst, '--out', outdir], capture_output=True)
    total = bad = order_only = 0
    shown = 0
    for f in files:
        name = f[:-4]
        sql_lines = [l for l in open(os.path.join(d, f), encoding='utf-8', errors='surrogateescape').read().split('\n') if l.strip()]
        want = split_out(open(os.path.join(d, name + '.ref'), encoding='utf-8', errors='surrogateescape').read())
        try:
            got = split_out(open(os.path.join(outdir, name + '.out'), encoding='utf-8', errors='surrogateescape').read())
        except FileNotFoundError:
            got = []
        for i, w in enumerate(want):
            total += 1
            g = got[i] if i < len(got) else '<missing>'
            if g != w and sorted(g.split('\n')) == sorted(w.split('\n')):
                order_only += 1
            elif g != w:
                bad += 1
                if shown < maxshow:
                    shown += 1
                    print('%s:%d: %s' % (name, i + 1, sql_lines[i] if i < len(sql_lines) else '?'))
                    print('   want: %r' % w[:400])
                    print('   got:  %r' % g[:400])
    print('mismatches: %d of %d statements (+%d row-order-only)' % (bad, total, order_only))


if __name__ == '__main__':
    main()
