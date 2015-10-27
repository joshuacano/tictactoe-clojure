(ns connectfour.models.validate
"Methods to validate Game as well as to check winners"
  (:use [connectfour.models.matrix])
  (:require [clojure.string :as string]))
        

(defn get-vertical [matrix x]
  (reduce str (map #((keyify x) %) matrix)))

(defn get-horizontal [matrix y]
  (reduce str (vals (get matrix y))))

(defn retrieve-diagonal [matrix topdown] 
  (let [returnmatrix []
        size (count (first matrix))]
    (for [y (range size)
          :let [xindex (- (- size 1) y)]]
      (if (= topdown 0) (conj returnmatrix (get-val matrix [y y]))
        ;else 
        (conj returnmatrix (get-val matrix [xindex y]))) 
      )))

(defn get-diagonal [matrix x]
  (first (apply map str (retrieve-diagonal matrix x))))

(defn x-won? [row]
  (let [size (count row)]
    (= row (string/join (repeat size "x")))))

(defn y-won? [row]
  (let [size (count row)]
    (= row (string/join (repeat size "y")))))

(defn all-taken? [matrix]
  (every? #(not (= 0 %)) (flatten (map vals matrix))))

(defn check-any-winners? [matrix who-func]
  (let [size (count (first matrix)) 
        outcomes (list 
                   (map #(get-horizontal matrix %) (range size))
                   (map #(get-vertical matrix %) (range size))
                   (map #(get-diagonal matrix %) (range 2)))]
    (some who-func (flatten outcomes))))

(defn did-game-draw? [matrix]
  (all-taken? matrix))

(defn did-somebody-win? [matrix]
  "Did anyone win this game?"
    (or 
      (check-any-winners? matrix x-won?)
      (check-any-winners? matrix y-won?)
      (did-game-draw? matrix )))

(defn did-x-win? [matrix]
    (check-any-winners? matrix x-won?))

(defn did-y-win? [matrix]
    (check-any-winners? matrix y-won?))

(defn who-won [matrix] 
  "returns a String indicating who won this game!"
  (cond 
    (did-x-win? matrix) "x"
    (did-y-win? matrix) "y"
    (did-game-draw? matrix) "Game ended in a Draw Holmes, SORRY"
    :else "Game is not over, Keep playing"))

