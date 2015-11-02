(ns connectfour.models.playbrain
  "Playbrain is the brains of the Game, determines how to play next move, get best move or stage a computer game"
  (:use [connectfour.models.matrix]
        [connectfour.models.validate])
  (:require [clojure.zip :as zip]
            [connectfour.models.minmax :as mm]
            [connectfour.models.alphabeta :as ab])
  (:gen-class))

(defn get-random-move
  "Get a Random move on Board"
  [matrix]
  (let [available (find-all-available matrix)
        size (count-available matrix)]
    (nth available (rand-int size))))

(defn play-random-move [matrix mark]
  (set-permanent-val (get-random-move matrix) mark))

(defn get-best-move
  "Get Best move on board"
  [board]
  (let [zipboard (mm/mk-zip (mm/create-node board))]
    (if (not (zip/branch? zipboard)) (:matrixpoint (zip/node zipboard)) ;Return Only move left!
      (let [childlocs (map mm/mk-zip (zip/children zipboard))
            scorelocs (map mm/find-score-node childlocs)
            childnodes (map zip/node scorelocs)
            childscores (map mm/make-minmax-keys childnodes)]
        (key (apply max-key val (into {} childscores))))))) ;(get-tree-keys-scores totalmat)))))

(defn play-best-move
  "Set Best move on board and return move to update front-end"
  [board mark]
  (let [move (ab/get-best-move board)]
    (do
      (set-permanent-val move mark)
      move)))

(defn play-computer-v-computer
  "Play computer v Computer till someone wins!"
  []
  (do
    (init-permanent-matrix 3)
    (loop [counter 1]
      (cond
        (did-somebody-win? (get-game-board)) (who-won (get-game-board)) ;If somebody win, call the who-won function and exit!
        (odd? counter)
        (do
          (println "Playing random, Move Count is " (str counter))
          (play-random-move (get-game-board) "x")
          (recur (inc counter)))
        :else (do
                (println "Playing Best Move, Move Count is " (str counter))
                (play-best-move (get-game-board) "y")
                (recur (inc counter)))))))


