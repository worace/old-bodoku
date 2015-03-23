(ns robodoku.core)
(require ['robodoku.parser :as 'parser])

(defn solved?
  [puzzle]
  (every? (fn [c] (= 1 c)) (map count (vals puzzle))))

(defn cols
  [puzzle]
  (let [size (Math/sqrt (count puzzle))]
    (map (fn [col-label]
           (select-keys puzzle (map (fn [row-label] (str col-label row-label)) (range 1 (+ 1 size)))))
         (parser/grid-letters size))))

(defn rows
  [puzzle]
  (let [size (Math/sqrt (count puzzle))]
    (map (fn [row-label]
           (select-keys puzzle (map (fn [col-label] (str col-label row-label)) (parser/grid-letters size))))
         (range 1 (+ 1 size)))))

(defn boxes
  [puzzle]
  (let [grid-size (int (Math/sqrt (count puzzle)))
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
  (let [grid-size (int (Math/sqrt (count puzzle)))]
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

