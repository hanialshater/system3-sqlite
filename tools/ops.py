import sys
pats = [' || ', ' << ', ' >> ', ' & ', ' | ', ' % ', ' / ', ' * ', ' == ', ' != ', ' <> ', '~', ' IS NOT ', ' NOT IN ',
        ' NOT BETWEEN ', ' LIKE ', ' GLOB ', 'ESCAPE', ' ISNULL', ' NOTNULL', ' NOT NULL', 'CAST(', ' COLLATE ',
        '+(', ' - ', ' + ', 'RTRIM', 'EXISTS', 'printf', 'round(', 'max(', 'IS DISTINCT', 'COLLATE BINARY', 'TRUE',
        'FALSE', 'zeroblob', 'sqlite_', 'ON CONFLICT', 'RETURNING', 'NATURAL', 'CROSS', 'INTERSECT', 'IFNULL', 'ifnull',
        'substring', 'rtrim', 'sign(', 'nullif(', 'iif(', 'DO NOTHING', 'OR REPLACE', 'OR IGNORE', 'OR ABORT', 'OR FAIL',
        'UPDATE OR', 'DEFAULT VALUES', 'WITHOUT', 'AUTOINCREMENT', 'VIEW', 'TRIGGER', 'ALTER', 'BEGIN', 'PRAGMA']
texts = [open(f, encoding='utf-8', errors='replace').read() for f in sys.argv[1:]]
for p in pats:
    print('%-16r' % p, *[t.count(p) for t in texts])
