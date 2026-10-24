"""String functions over text with random (often invalid) UTF-8. Usage: gen_utf.py outdir count seed"""
import os
import random
import sys

out, count, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
random.seed(seed)
os.makedirs(out, exist_ok=True)
pieces = ['61', '62', 'c3', 'a9', 'b3', 'e4', '80', 'fa', 'ff', 'c0', 'e6', '97', 'a5', 'f0', '9f', '98', '80', '25', '5f', '20', '41', '00']


def t():
    n = random.randint(0, 6)
    return "CAST(x'%s' AS TEXT)" % ''.join(random.choice(pieces) for _ in range(n))


for i in range(count):
    lines = []
    for _ in range(40):
        a, b, c = t(), t(), t()
        f = random.choice([
            'length(%s)' % a, 'hex(upper(%s))' % a, 'hex(lower(%s))' % a,
            'hex(substr(%s, %d, %d))' % (a, random.randint(-4, 4), random.randint(-3, 4)),
            'instr(%s, %s)' % (a, b), 'hex(replace(%s, %s, %s))' % (a, b, c), 'hex(trim(%s, %s))' % (a, b),
            'hex(ltrim(%s, %s))' % (a, b), 'hex(rtrim(%s, %s))' % (a, b), 'hex(trim(%s))' % a, 'quote(%s)' % a,
            'unicode(%s)' % a, '%s LIKE %s' % (a, b), '%s GLOB %s' % (a, b), '%s < %s' % (a, b),
            '%s = %s COLLATE NOCASE' % (a, b), 'hex(%s || %s)' % (a, b), 'typeof(%s)' % a,
            "%s LIKE '%%' || %s || '%%'" % (a, b), 'hex(char(unicode(%s)))' % a,
        ])
        lines.append('SELECT %s;' % f)
    open(os.path.join(out, 'utf-%04d.sql' % i), 'w').write('\n'.join(lines) + '\n')
