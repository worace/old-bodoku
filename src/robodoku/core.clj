(ns robodoku.core)
(require ['robodoku.parser :as 'parser])

(defn puzzle-size
  [puzzle]
  (int (Math/sqrt (count puzzle))))

(defn puzzle-possibilities
  [puzzle]
  (map str (seq (parser/cell-possibilities (puzzle-size puzzle)) ) ))

(defn solved?
  [puzzle]
  (every? (fn [c] (= 1 c)) (map count (vals puzzle))))

(defn cols
  [puzzle]
  (let [size (puzzle-size puzzle)]
    (map (fn [col-label]
           (select-keys puzzle (map (fn [row-label] (str col-label row-label)) (range 1 (+ 1 size)))))
         (parser/grid-letters size))))

(defn rows
  [puzzle]
  (let [size (puzzle-size puzzle)]
    (map (fn [row-label]
           (select-keys puzzle (map (fn [col-label] (str col-label row-label)) (parser/grid-letters size))))
         (range 1 (+ 1 size)))))

(defn boxes
  [puzzle]
  (let [grid-size (puzzle-size puzzle)
        box-size (int (Math/sqrt grid-size))
        col-groups (partition box-size (parser/grid-letters grid-size))
        row-groups (partition box-size (range 1 (+ 1 grid-size)))]
    (map (fn [cells] (select-keys puzzle cells))
         (for [row-group row-groups col-group col-groups]
           (for [row row-group col col-group]
             (str col row))))))

(defn units
  [puzzle]
  (apply conj (apply conj (rows puzzle) (cols puzzle)) (boxes puzzle)))

(defn contradictory?
  [puzzle]
  (let [grid-size (puzzle-size puzzle)]
    (not (every? (fn [u]
                (= (parser/cell-possibilities grid-size) (apply str (sort (vals u)))))
              (units puzzle)))))

(defn peers-map
  [puzzle]
  (reduce (fn [peers cell]
            (assoc peers cell
                   (remove (fn [peer] (= peer cell))
                           (distinct
                             (mapcat keys
                                     (filter
                                       (fn [unit] (contains? unit cell))
                                       (units puzzle)))))))
          {} (keys puzzle)))


;Cases
;value is not a possibility -- return vals (TODO - need special case for this?)
;value is last possibility -- none remain, so return false (contradiction)
;eliminating val leaves 1 possibility -- remove remaining possibility from peers of cell
  ; if those are all successful - return new puzzle
  ; else return false (elimination failed)
(defn eliminate
  [puzzle peers-map cell value]
  ;(println (str "attempt to elim val " value " from cell " cell " which currently has " (get puzzle cell)))
  (if (not (.contains (get puzzle cell) value))
    ;value is not an option for cell (already eliminated); return existing puzzle
    ;(println (str "did not find value " value " in values for cell " cell ": " (get puzzle cell)))
    puzzle
    (let [new-cell-value (clojure.string/replace (get puzzle cell) value "")]
      (if (empty? new-cell-value)
        false ;no remaining possibilities - introduced contradiction - return false
        (if (= 1 (count new-cell-value))
          ;1 remaining poss must be solution; elim it from all peers
          (loop [peers (get peers-map cell)
                 puzzle (assoc puzzle cell new-cell-value)]
            (if (empty? peers)
              puzzle
              (let [next-iter (eliminate puzzle peers-map (first peers) new-cell-value)]
                (if next-iter
                  (recur (rest peers) next-iter)
                  false))))
          (assoc puzzle cell new-cell-value))))))

(defn assign
  [puzzle peers-map cell value]
  ;(println (str "assign val " value " to cell " cell " currently has possibilities " (get puzzle cell)))
  (loop [values-to-elim (remove #(= % value) (puzzle-possibilities puzzle))
         puzzle puzzle]
    (if (empty? values-to-elim)
      puzzle
      (if puzzle
        (recur (rest values-to-elim) (eliminate puzzle peers-map cell (first values-to-elim)))
        false))))

(defn constrain [puzzle-with-values]
  ;(println (str "need to fill constraints in puzzle: " puzzle-with-values))
  ;(println (str "Will copy into empty puzzle: " (parser/empty-puzzle (puzzle-size puzzle-with-values))))
  (let [peers-map (peers-map puzzle-with-values)]
    (loop [cells (keys puzzle-with-values)
           puzzle (parser/empty-puzzle (puzzle-size puzzle-with-values))]
      (if (empty? cells)
        puzzle
        (let [cell (first cells)
              value (get puzzle-with-values cell)]
          (if (= 1 (count value))
            (recur (rest cells) (assign puzzle peers-map cell value))
            (recur (rest cells) puzzle))))) ))

(defn unsolved-cells [puzzle]
  (filter (fn [kv] (> (count (last kv)) 1)) puzzle))

(defn min-possibilities-cell [puzzle]
  (first (first (sort-by (fn [kv] (count (last kv)) ) (unsolved-cells puzzle)))))

(defn search [puzzle peers]
  (if (= false puzzle)
    false
    (if (and (solved? puzzle) (not (contradictory? puzzle)))
      puzzle
      (let [next-cell (min-possibilities-cell puzzle)]
        (some (fn [possibility]
                (search (assign puzzle peers next-cell possibility) peers))
              (map str (seq (get puzzle next-cell)))) ))))

(defn display-row [row]
  (clojure.string/join "" (map last (sort-by (fn [kv] (first kv)) row))))

(defn display [puzzle]
  (clojure.string/join "\n" (map display-row (rows puzzle))))





;Assigning values to cells
; - set the new value as the only remaining possibility for the cell
; - remove the new value from the possibilities for each of the cell's peers
; - if any of these peers now has only 1 value, assign that value to the cell (recursive; should repeat step 2)
