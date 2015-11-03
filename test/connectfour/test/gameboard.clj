(ns connectfour.test.gameboard
  (:require [clojure.test :refer :all]
            [connectfour.models.matrix :refer :all]
            [connectfour.models.validate :refer :all]
            [connectfour.models.playbrain :refer :all]))

(defn empty-board? 
  [board]
  (every? #(if (number? %) (zero? %) false) (flatten (map vals board))))

(deftest test-initialize
  (testing "Main Board"
    (let [board (init-permanent-matrix 3)]
      (is (empty-board? (get-game-board)))))

  (testing "Test that set-val does not alter main board"
    (let [board (init-permanent-matrix 3)]
      (do
        (set-val (get-game-board) [0 0] "x")
        (is (empty-board? (get-game-board))))))
  
  (testing "Test that set-permanent val does alter main board"
    (let [board (init-permanent-matrix 3)]
      (do
        (set-permanent-val [0 0] "x")
        (is (not (empty-board? (get-game-board))))))))

(deftest test-validate 
  (testing "Testing that X can win the game!"
    (do 
      (init-permanent-matrix 3)
      (set-permanent-val [0 2] "x")
      (set-permanent-val [1 2] "y")
      (set-permanent-val [0 1] "x")
      (set-permanent-val [2 2] "y")
      (set-permanent-val [0 0] "x")
      (is (= "x" (did-x-win? (get-game-board))))))
  
  (testing "Testing that y can win the game!"
    (do 
      (init-permanent-matrix 3)
      (set-permanent-val [0 2] "y")
      (set-permanent-val [1 2] "x")
      (set-permanent-val [0 1] "y")
      (set-permanent-val [2 2] "x")
      (set-permanent-val [0 0] "y")
      (is (= "y" (did-y-win? (get-game-board))))))
  
  (testing "Testing that there is a draw"
    (do 
      (init-permanent-matrix 3)
      (set-permanent-val [0 0] "x")
      (set-permanent-val [0 1] "y")
      (set-permanent-val [0 2] "x")
      (set-permanent-val [1 0] "y")
      (set-permanent-val [1 2] "x")
      (set-permanent-val [1 1] "y")
      (set-permanent-val [2 1] "x")
      (set-permanent-val [2 2] "y")
      (set-permanent-val [2 0] "x")
      (is (= "draw" (who-won? (get-game-board)))))))
   
(deftest test-playbrain 
  (testing "Game will never end in X Winning"
    (do 
        (let [result (repeatedly 10 #(str (play-computer-v-computer 3)))]
          (is (not-any? #{"x"} result))))))
  
  ;This is not working currently
  ;(testing "Test game for larger size than 3"
  ;  (do 
  ;      (let [integers (take 10 (repeatedly #(+ 1 (rand-int 10))))
  ;            result (map play-computer-v-computer integers)]
  ;        (is (not-any? #{"x"} result))))))

