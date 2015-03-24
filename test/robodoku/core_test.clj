(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [robodoku.core :refer :all]
            [robodoku.parser :as parser]))

(defn test-puzzle
  [puzz-name]
  (parser/parse-puzzle (str puzz-name ".txt")))

(def tbt-with-blank (test-puzzle "two_by_two_with_blank"))
(def tbt-solved (test-puzzle "two_by_two"))
(def fbf-unsolved (test-puzzle "four_by_four_with_blank"))
(def fbf-hard (test-puzzle "four_by_four_hard"))
(def fbf-solved (test-puzzle "four_by_four"))
(def nbn-solved (test-puzzle "nine_by_nine"))
(def nbn-cont (test-puzzle "nine_by_nine_contradictory"))


(deftest test-checking-puzzle-solvedness
  (testing "a puzzle is not solved if there are unsolved cells"
    (is (not (solved? tbt-with-blank))))
  (testing "filled in puzzle is solved"
    (is (solved? tbt-solved))))

(deftest test-checking-puzzle-contradictions
  (testing "a solved puzzle is not contradictory"
    (is (not (contradictory? fbf-solved))))
  (testing "a puzzle is contradictory if a value is repeated within a row or col"
    (is (contradictory? nbn-cont))))

(deftest test-finding-rows
  (testing "it finds rows of a puzzle"
    (is (= [{"A1" "1" "B1" "2"} {"A2" "2" "B2" "1"}] (rows tbt-solved))))
  (testing "it finds rows of 4x4")
    (is (= {"A1" "3" "B1" "2" "C1" "4" "D1" "1"} (first (rows fbf-solved)))))

(deftest test-finding-cols
  (testing "it finds cols of a puzzle"
    (is (= [{"A1" "1" "A2" "2"} {"B1" "2" "B2" "1"}] (cols tbt-solved))))
  (testing "finds cols of 4x4"
    (is (= {"A1" "3" "A2" "4" "A3" "1" "A4" "2"} (first (cols fbf-solved))))))

(deftest test-finding-boxes
  (testing "it finds boxes of a puzzle"
    (is (= {"A1" "3" "B1" "2" "A2" "4" "B2" "1"} (first (boxes fbf-solved) ))))
  (testing "it finds boxes of 9x9"
    (is (= {"A1" "8" "B1" "2" "C1" "6" "A2" "7" "B2" "1" "C2" "5" "A3" "3" "B3" "9" "C3" "4"}
           (first (boxes nbn-solved) )))))

(deftest test-finding-all-units
  (testing "it combines rows, boxes, and cols"
    (is (= 27 (count (units nbn-solved))))
    (is (every? (fn [unit] (= 9 (count unit))) (units nbn-solved)))))

(deftest test-finding-peers-of-a-cell
  (testing "it finds peers for A4 in 9x9"
    (is (= (sort ["A1" "A2" "A3" "A5" "A6" "A7" "A8" "A9"
              "B4" "C4" "D4" "E4" "F4" "G4" "H4" "I4"
              "B5" "C5" "B6" "C6"]) (sort (get (peers-map nbn-solved) "A4"))))))

(deftest test-setting-up-initial-constraints
  (testing "it takes a parsed puzzle and propagates initial constraints"
    (is (= "34" (get (constrain fbf-hard) "A1")))))

(deftest test-assigning-a-value-to-a-cell
  (testing "it returns a new map with the value filled in"
    (is (= "3" (get (assign fbf-unsolved "B4" "3") "B4")))))

(deftest test-elimination-contradictions
  (testing "eliminate returns false if last value is eliminated"
    (is (= false (eliminate fbf-solved "A1" "3"))))
  (let [constrained (assoc (assoc fbf-unsolved "B4" "13") "C4" "13")]
    (testing "eliminate returns false if we introduce contradiction for peer"
      (is (= false (eliminate constrained "B4" "3")))));3 is solution for B4; eliminating it should fail
  )

(deftest test-eliminating-already-eliminated-val
  (testing "eliminate returns existing puzzle if value is not a possibility for cell"
    (is (= fbf-solved (eliminate fbf-solved "A1" "2")))))

(deftest finding-unsolved-cells
  (testing "finds cells with more than 1 remaining possibilities"
    (is (= ["C4" "B4"]) (unsolved-cells fbf-unsolved))))

(println "*****************")
(println (unsolved-cells fbf-unsolved ))
(deftest test-finding-minimum-possibilities-cell
  (testing "it finds the unsolved cell which has fewest remaining possibilities"
    (is (contains? (set ["D2" "B4"]) (min-possiblities-cell fbf-unsolved)))))

(deftest test-solving-a-puzzle
  (testing "solve recursively searches for a solution until it finds one"))

;3241
;4132
;1423
;2 14 -> B4 blank; should be 3
