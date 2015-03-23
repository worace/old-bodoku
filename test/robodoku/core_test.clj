(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [robodoku.core :refer :all]))

(deftest constructing-puzzle-file-paths
  (testing "it finds resource path given source dir and puzzle file"
    (is (= "puzzles/two_by_two.txt" (puzzle-file "two_by_two.txt" "puzzles"))))
  (testing "it defaults dir to puzzles"
    (is (= "puzzles/two_by_two.txt" (puzzle-file "two_by_two.txt")))))

;(deftest reading-sudoku-files
  ;(testing "it reads a puzzle file and turns it into a map"
    ;(is (= {:a1 "1" :a2 "2" :b1 "3" :b4 "4"} (read-puzzle (puzzle-file "two_by_two.txt"))))))

(deftest test-finding-cell-possibilities
  (testing "it generates string of possibilities for grid of requested size"
    (is (= "1234" (cell-possibilities 4))))
  (testing "it defaults to 9"
    (is (= "123456789" (cell-possibilities)))))

(deftest test-finding-grid-letters
  (testing "it finds appropriate number of grid letters"
    (is (= ["A" "B" "C" "D"] (grid-letters 4))))
  (testing "defaults to 9"
    (is (= ["A" "B" "C" "D" "E" "F" "G" "H" "I"] (grid-letters)))))

(deftest test-finding-cell-labels
  (testing "it crosses rows and cols to generate cell labels"
    (is (= ["A1" "A2" "B1" "B2"] (cell-labels 2))))
  (testing "defaults to 9"
    (is (= 81 (count (cell-labels))))))
