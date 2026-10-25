(ns sqlclj.guard
  "Cooperative abort for scripts that overrun their time budget: main abandons the script's thread and marks it
  here; hot loops call (check!) so the abandoned thread stops soon instead of burning CPU beside the next script.")

(def ^java.util.Set killed (java.util.concurrent.ConcurrentHashMap/newKeySet))

(defn check! []
  (when (and (not (.isEmpty killed)) (.contains killed (Thread/currentThread)))
    (throw (Error. "script aborted: time budget exceeded"))))
