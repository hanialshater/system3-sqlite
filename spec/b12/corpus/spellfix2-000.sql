SELECT word, distance, matchlen FROM demo WHERE word MATCH 'amstedam*' AND top=3 ORDER BY +word;
SELECT word, distance, matchlen FROM demo WHERE word MATCH 'amstedam*' AND top=3 AND distance <= 100 ORDER BY +word;
SELECT word, distance, matchlen FROM demo WHERE word MATCH 'amstedam*' AND distance <= 100 ORDER BY +word;
SELECT count(*) FROM demo WHERE word MATCH 'amstedam*' AND distance <= 100; SELECT count(*) FROM demo WHERE word MATCH 'amstedam*' AND distance <= 100 AND top=20;
SELECT word, distance, matchlen FROM demo WHERE word MATCH 'amstedam*' AND distance <= 100 ORDER BY distance, word;
SELECT word, distance, matchlen FROM demo WHERE word MATCH 'amstedam*' AND distance <= 100 AND top=20 ORDER BY distance, word;
