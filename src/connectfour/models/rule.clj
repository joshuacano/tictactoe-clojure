(ns connectfour.models.rule
  (:use [clojure.string] 
        [connectfour.models.matrix] 
        [connectfour.models.validate]) )

(defn count-moves [matrix]
  (count (filter #(not (= 0 %)) (flatten (map vals matrix)))))

;(defprotocol Game
;  (win [x] "Have I won the game")
;  (score [x] "My possible score"))

;(defrecord tictac [matrix]
;  (Game
;    (win [x] 
;         (count-moves ))
;    (score [x] 2)))

