"""Statement shape inventory: python3 tools/shapes.py file..."""
import re
import sys
import collections

c = collections.Counter()
for fn in sys.argv[1:]:
    for line in open(fn, encoding='utf-8', errors='replace'):
        line = line.strip()
        if not line:
            continue
        s = re.sub(r"x'[0-9a-fA-F]*'", 'B', line)
        s = re.sub(r"'(?:[^']|'')*'", 'S', s)
        s = re.sub(r'\b\d+(\.\d+)?(e[-+]?\d+)?\b', 'N', s)
        s = re.sub(r'\bc\d\b', 'c', s)
        s = re.sub(r'\bt\d\b', 't', s)
        s = re.sub(r'\bi\d+\b', 'i', s)
        # collapse first 6 words
        words = s.split()
        key = ' '.join(words[:5])
        c[key] += 1
for k, v in c.most_common():
    print(v, k)
