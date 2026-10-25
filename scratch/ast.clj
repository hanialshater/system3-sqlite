(require '[sqlclj.parser :as p])
(doseq [s ["SELECT a FROM t1 WHERE a=1 AND b>2 AND c IN (1,2) AND d BETWEEN 1 AND 2 AND e LIKE 'x%' AND f IS NULL AND g IS NOT NULL AND h=-1 AND i COLLATE nocase = 'a' AND (j=1 OR k=2) ORDER BY a DESC, b LIMIT 5"]]
  (prn (p/next-statement (p/parse-statements s))))
