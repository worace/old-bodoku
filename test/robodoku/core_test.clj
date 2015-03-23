(ns robodoku.core-test
  (:require [clojure.test :refer :all]
            [robodoku.core :refer :all]))

(deftest constructing-puzzle-file-paths
  (testing "it finds resource path given source dir and puzzle file"
    (is (= "puzzles/two_by_two.txt" (puzzle-file "two_by_two.txt" "puzzles"))))
  (testing "it defaults dir to puzzles"
    (is (= "puzzles/two_by_two.txt" (puzzle-file "two_by_two.txt")))))

(deftest reading-sudoku-files
  (testing "it reads a puzzle file and turns it into a map"
    (is (= {:a1 "1" :a2 "2" :b1 "3" :b4 "4"} (read-puzzle (puzzle-file "two_by_two.txt"))))))
