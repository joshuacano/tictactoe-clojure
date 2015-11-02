(ns connectfour.models.validate
"Methods to validate Game as well as to check winners"
  (:use [connectfour.models.matrix])
  (:require [clojure.string :as string]))
        
(defn get-vertical 
  "Get one column of board"
  [matrix x]
  (reduce str (map #((keyify x) %) matrix)))

(defn get-horizontal 
  "Get one row of board" 
  [matrix y]
  (reduce str (vals (get matrix y))))

(defn retrieve-diagonal 
  "Retrieve diagonal rows of board"
  [matrix topdown] 
  (let [returnmatrix []
        size (count (first matrix))]
    (for [y (range size)
          :let [xindex (- (- size 1) y)]]
      (if (= topdown 0) (conj returnmatrix (get-val matrix [y y]))
        ;else 
        (conj returnmatrix (get-val matrix [xindex y]))) 
      )))

(defn get-diagonal 
  "Wrapper function to format diagonal row" 
  [matrix x]
  (first (apply map str (retrieve-diagonal matrix x))))

(defn x-won? 
  "Did x / User win on any one row/column??"
  [row]
  (let [size (count row)]
    (= row (string/join (repeat size "x")))))

(defn y-won?
 "Did y / Computer win on any one row/column? "
 [row]
  (let [size (count row)]
    (= row (string/join (repeat size "y")))))

(defn all-taken? 
  "Are all cells of a board taken"
  [matrix]
  (every? #(not (= 0 %)) (flatten (map vals matrix))))

(defn check-any-winners? 
  "Check if there are any winners, this function is a bit obtuse"
  [matrix who-func]
  (let [size (count (first matrix)) 
        outcomes (list 
                   (map #(get-horizontal matrix %) (range size))
                   (map #(get-vertical matrix %) (range size))
                   (map #(get-diagonal matrix %) (range 2)))]
    (some who-func (flatten outcomes))))

(defn did-somebody-win? [matrix]
  "Did anyone win this game?"
    (or 
      (check-any-winners? matrix x-won?)
      (check-any-winners? matrix y-won?)
      (all-taken? matrix)))

(defn did-x-win? 
  "Check if x won for entire board"
  [matrix]
    (check-any-winners? matrix x-won?))

(defn did-y-win? 
  "Check if y won for entire board"
  [matrix]
    (check-any-winners? matrix y-won?))

(defn who-won [matrix] 
  "returns a String indicating who won this game!"
  (cond 
    (did-x-win? matrix) "x"
    (did-y-win? matrix) "y"
    (all-taken? matrix) "draw"
    :else "Game is not over, Keep playing"))

(defn score
  "Return score of Matrix calculation"
  ([matrix]
   (cond
     (did-y-win? matrix) 10
     (did-x-win? matrix) -10
     :else 0))
  ([matrix depth] ;Handle depth calculations too
   (cond
     (did-y-win? matrix) (- 10 depth)
     (did-x-win? matrix) (- depth 10 )
     :else 0)))

