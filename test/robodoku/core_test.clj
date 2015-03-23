(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [robodoku.core :refer :all]
            [robodoku.parser :as parser]))

(defn test-puzzle
  [puzz-name]
  (parser/parse-puzzle (str puzz-name ".txt")))

(def tbt-with-blank (test-puzzle "two_by_two_with_blank"))
(def tbt-solved (test-puzzle "two_by_two"))
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
