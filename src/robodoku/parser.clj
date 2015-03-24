(ns robodoku.parser)

(defn read-puzzle
  "Returns seq of strings representing puzzle cell values (81 for 9x9 grid)"
  [filepath]
  (rest (clojure.string/split
      (clojure.string/replace
        (slurp (.getFile (clojure.java.io/resource filepath))) #"\n" "")
      #"")))

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
  ([size] (for [row (range 1 (+ 1 size)) col (grid-letters size)]
             (clojure.string/join "" [col row])))
  ([] (cell-labels 9)))

(defn fill-cells-with-possibilities
  [cell-values grid-size]
  (map (fn [cell-value]
         (if (= " " cell-value)
           (cell-possibilities grid-size)
           cell-value)) cell-values))

(defn parse-puzzle
  [filename]
  (let [cell-values (read-puzzle (puzzle-file filename))
        size (int (Math/sqrt (count cell-values)))]
    (zipmap (cell-labels size) (fill-cells-with-possibilities cell-values size))))

(defn empty-puzzle [size]
  (zipmap (cell-labels size) (iterate (fn [i] (cell-possibilities size)) (cell-possibilities size))))
