(ns robodoku.core)

(defn read-puzzle
  [filepath]
  (slurp (.getFile (clojure.java.io/resource filepath))))

(defn puzzle-file
  ([puzzle resource-dir] (clojure.string/join "/" [resource-dir puzzle]))
  ([puzzle] (puzzle-file puzzle "puzzles")))

(defn cell-possibilities
  ([size] (clojure.string/join "" (range 1 (+ 1 size))))
  ([] (cell-possibilities 9)))

(defn grid-letters
  ([size] (map str (map char (range 65 (+ 65 size)))))
  ([] (grid-letters 9)))

(defn cell-labels
  ([size] (for [col (grid-letters size) row (range 1 (+ 1 size))]
             (clojure.string/join "" [col row])))
  ([] (cell-labels 9)))
