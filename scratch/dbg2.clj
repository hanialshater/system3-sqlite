(require '[sqlclj.parser :as p])
(prn (:from (:body (:select (p/next-statement (p/parse-statements "SELECT * FROM t1 LEFT JOIN t2 ON t1.b=t2.y WHERE t1.a=1"))))))
