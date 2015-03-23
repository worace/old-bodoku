(ns robodoku.core)

(defn read-puzzle
  [filepath]
  (slurp (.getFile (clojure.java.io/resource filepath))))

(defn puzzle-file
  ([puzzle resource-dir] (clojure.string/join "/" [resource-dir puzzle]))
  ([puzzle] (puzzle-file puzzle "puzzles")))
