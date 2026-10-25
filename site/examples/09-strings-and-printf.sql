SELECT printf('%5.2f|%-6s|%05d|%x|%q', 3.14159, 'ab', 42, 255, 'it''s');
SELECT upper('clojure'), length('naïve'), substr('database', -4), replace('a-b-c', '-', '+'), instr('referee', 'fer');
SELECT trim('  padded  '), ltrim('xxabc', 'x'), 'abc' LIKE 'A%', 'abc' GLOB 'A*', hex('SQL'), quote('O''Hare');
