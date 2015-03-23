(ns robodoku.parser-test
  (:require [clojure.test :refer :all]
            [robodoku.parser :refer :all]))

(deftest constructing-puzzle-file-paths
  (testing "it finds resource path given source dir and puzzle file"
    (is (= "puzzles/two_by_two.txt" (puzzle-file "two_by_two.txt" "puzzles"))))
  (testing "it defaults dir to puzzles"
    (is (= "puzzles/two_by_two.txt" (puzzle-file "two_by_two.txt")))))

(deftest test-read-puzzle
  (testing "it returns seq of chars for puzzle"
    (is (= ["1" "2" "2" "1"] (read-puzzle (puzzle-file "two_by_two.txt"))))))

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

(deftest test-parsing-a-puzzle-file
  (testing "it returns map of char label to char value"
    (is (= {"A1" "1" "A2" "2" "B1" "2" "B2" "1"} (parse-puzzle "two_by_two.txt"))))
  (testing "a blank square retains all possible values for that puzzle"
    (is (= {"A1" "12" "A2" "2" "B1" "2" "B2" "1"} (parse-puzzle "two_by_two_with_blank.txt")))))
