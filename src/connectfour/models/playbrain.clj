(ns connectfour.models.playbrain
  (:use [connectfour.models.matrix] 
        [connectfour.models.validate])
        
 (:require [clojure.zip :as zip] )
 (:gen-class))

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

 (defn create-node 
  "Create tree of each game board with score associated at point"
  ([matrix] {:matrix matrix :id (gensym "tic") :score (score matrix) :depth -1}) ;Create root
  ([matrix [x y] parent depth] {:matrix matrix :matrixpoint [x y] :id (gensym "tic") :parent parent :score (score matrix depth) :depth depth})) ;Create child nodes

(defn branch-tic [node]
  "Make branch for tic tac toe"
  (let [matrix (:matrix node)
        avail (find-all-available matrix)
        has-kids (pos? (count avail))
        no-score (zero? (:score node))]
    (and has-kids no-score)))

(defn children-tic [node]
  "Make children for tic tac toe"
  (let [matrix (:matrix node)
        id (:id node)
        depth (+ 1 (:depth node))
        avail (find-all-available matrix)
         mark (if (odd? depth) "x" "y")]
    (map #(create-node (set-val matrix % mark) % id depth) avail)))

(defn node-tic 
  [node children]
  (cond
    (nil? node) nil
    (map? node) (assoc node :inputs (map #(hash-map :parent (:parent %) :score (:score %) :minmax (:minmax %) :matrixpoint (:matrixpoint %)) children))
    (vector? node) (into [] children)
    :else node))

(defn mk-zip [root]
    (zip/zipper branch-tic children-tic node-tic root))

(defn get-minmax-score [depth coll]
  "Get MinMax algorithm result for a collection of integers"
  (if (odd? depth) (apply max coll) (apply min coll)))

(defn retrieve-minmax-score [node]
  (if (nil? (:minmax node)) (:score node) (:minmax node)))


(defn zip-last-node 
  [loc] 
  (if (zip/branch? loc) (recur (-> loc zip/down)) loc))

(defn make-fake-zip []
   (let [zd (zip/down (mk-zip (create-node (get-game-board))))]
     (zip/right (zip-last-node zd))))

(defn equal-matrix 
  [aloc bloc]
  (= (:matrix (zip/node aloc)) (:matrix (zip/node bloc))))


(defn set-minmax-score 
  [node score]
   (assoc node :minmax score))

(defn set-minmax-node 
  ([loc] (if (nil? loc) loc
   (set-minmax-node loc (:score (zip/node loc)))))
  ([loc score]
  (if (nil? loc) loc
      (zip/edit loc set-minmax-score score))))

(defn all-set? [coll]
  (do 
  (every? #(contains? % :minmax ) coll)))


(defn extract-children [loc]
  (if (nil? loc) nil  
  (let [node (zip/node loc)]
    (if (contains? node :inputs) 
      (:inputs node)
      (if (not (zip/branch? loc)) loc (zip/children loc))))))

(defn collect-min-max 
  [loc]
  (let [depth (:depth (zip/node loc))
        children (if (not (zip/branch? loc)) nil (extract-children loc))
        minmax (if (nil? children) (:score (zip/node loc)) (get-minmax-score depth (map :minmax children)))]
    (set-minmax-node loc minmax)))

(defn gimme-new-loc
  [loc]
    ;(println "LEAVES: " (zip/node loc))
  (cond 
    (nil? loc) nil
    (zip/branch? loc) loc
    :else 
    (let [no-siblings (zero? (count (zip/rights loc)))
          newnode (set-minmax-node loc)]
        ;No Kids and No Siblings
        (if no-siblings (zip/up newnode)
          ;No Kids but has Siblings
          (zip/right newnode)))))
  
  (defn find-score-node
    ([initial-loc] (if (nil? (zip/down initial-loc)) (find-score-node initial-loc initial-loc)
                     (find-score-node initial-loc (zip/down initial-loc))))
    ([initial-loc loc]
    ;(println "PARENTS: " (zip/node loc))
  (cond
    (nil? loc) nil
    (equal-matrix initial-loc loc) (collect-min-max loc)
    (nil? (zip/up loc)) loc ;WE AT THE TOP!!
    (not (zip/branch? loc)) (recur initial-loc (gimme-new-loc loc))
    (all-set? (extract-children loc)) 
    (let [siblings (zip/rights loc)
          newnode (collect-min-max loc)]
    ;(do 
      ;(println "I'm ALL SET")
      ;(println "THIS NODE IS SET? " (zip/node newnode))
     (if (zero? (count siblings)) (recur initial-loc (zip/up newnode))
       (recur initial-loc (zip/right newnode))))
    :else 
           ; (println "children " (extract-children loc))
           ; (println "I'm NOT SET FOR " (extract-children loc))
           (recur initial-loc (zip/down loc)))))
            
(defn get-random-move [matrix]
  "Get a Random move on Board"
  (let [available (find-all-available matrix)
        size (count available)]
    (nth available (rand-int size))))

(defn play-random-move [matrix mark]
  (set-permanent-val (get-random-move matrix) mark))

(defn make-minmax-keys [node]
"Makes a map with a key of possible move and its associated score"
  {(:matrixpoint node) (:minmax node)})

(defn get-best-move [board]
  "Get Best move on board"
  (let [zipboard (mk-zip (create-node board))]
    (if (not (zip/branch? zipboard)) (:matrixpoint (zip/node zipboard)) ;Return Only move left!
      (let [childlocs (map mk-zip (zip/children zipboard))
            scorelocs (map find-score-node childlocs)
            childnodes (map zip/node scorelocs)
            childscores (map make-minmax-keys childnodes)]
        (key (apply max-key val (into {} childscores))))))) ;(get-tree-keys-scores totalmat)))))

(defn play-best-move [board mark]
  "Set Best move on board"
  (set-permanent-val (get-best-move board) mark))

(defn play-computer-v-computer [] 
  "Play computer v Computer till someone wins!" 
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

(def best-move-maps (atom []))

(defn set-best-move-map 
  [board [x y] mark]
    (let [newboard (set-val board [x y] mark)
          matrixpoint (get-best-move newboard)]
      (hash-map :index [x y] :matrix newboard :matrix-point matrixpoint)))

(defn set-move-map
  [board mark]
  (let [cnt (count (find-all-available board))
        size (* (count board) (count board))
        mark (if (odd? cnt) "x" "y")]
    (if (zero? cnt) mark
      (for [avail (find-all-available board)]
        (set-best-move-map board avail mark)))))


