(ns connectfour.models.minmax
  "Minmax Algorithm creates a minmax search tree to tell Playbrain how to move next"
  (:use [connectfour.models.matrix] 
        [connectfour.models.validate])
  (:require [clojure.zip :as zip] )
  (:gen-class))

(defn create-node 
  "Create tree of each game board with score associated at point"
  ([matrix] {:matrix matrix :score (score matrix) :depth -1 :alpha (Double/NEGATIVE_INFINITY) :beta (Double/POSITIVE_INFINITY)}) ;Create root
  ([matrix [x y] parent depth] {:matrix matrix :matrixpoint [x y] :id (gensym "tic") :parent parent :score (score matrix depth) :depth depth
                                })) ;Create child nodes

(defn branch-tic 
  "Make branch for tic tac toe"
  [node]
  (let [matrix (:matrix node)
        has-kids (pos? (count-available matrix))
        no-score (zero? (:score node))]
    (and has-kids no-score)))

(defn children-tic 
  "Make children for tic tac toe"
  [node]
  (let [matrix (:matrix node)
        id (:id node)
        depth (+ 1 (:depth node))
        avail (find-all-available matrix)
        already-kids? (not (nil? (:inputs node)))
        mark (if (odd? depth) "x" "y")
        ]
    (if already-kids? (:inputs node)
      (map #(create-node (set-val matrix % mark) % id depth) avail))))

(defn node-tic 
  "Make a new node for tic tac toe" 
  [node children]
  (cond
    (nil? node) nil
    (map? node) (assoc node :inputs (map #(hash-map :parent (:parent %) :depth (:depth %) :matrix (:matrix %) :score (:score %) :minmax (:minmax %) :alpha (:alpha %) :beta (:beta %) :matrixpoint (:matrixpoint %)) children))
    (vector? node) (into [] children)
    :else node))

(defn mk-zip 
  "Master function to create board"
  [root]
  (zip/zipper branch-tic children-tic node-tic root))

(defn get-minmax-score 
  "Get MinMax algorithm result for a collection of integers"
  [depth coll]
  (if (odd? depth) (apply max coll) (apply min coll)))

(defn retrieve-minmax-score [node]
  (if (nil? (:minmax node)) (:score node) (:minmax node)))

(defn zip-last-node 
  "Helper function to zip to last node" 
  [loc] 
  (if (zip/branch? loc) (recur (-> loc zip/down)) loc))

(defn make-fake-zip 
  "This should be moved to testing functions"
  []
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

(defn all-set? 
  "A method to check if all items in a collection have had score set"
  [coll]
  (do 
    (every? #(contains? % :minmax ) coll)))

(defn extract-children 
  "Returns children of any particular node on tree"
  [loc]
  (if (nil? loc) nil  
    (let [node (zip/node loc)]
      (if (contains? node :inputs) 
        (:inputs node)
        (if (not (zip/branch? loc)) loc (zip/children loc))))))

(defn collect-min-max 
  "Collect Minmax score for all children and set node to appropriate score"
  [loc]
  (let [depth (:depth (zip/node loc))
        children (if (not (zip/branch? loc)) nil (extract-children loc))
        minmax (if (nil? children) (:score (zip/node loc)) (get-minmax-score depth (map :minmax children)))]
    (set-minmax-node loc minmax)))

(defn gimme-new-loc 
  "Guiding function to indicate direction of next node in search tree"
  [loc]
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

(def scoreatom (atom 0))

(defn find-score-node
  "find score for any tree of possible moves"
  ([initial-loc] (if (nil? (zip/down initial-loc)) (find-score-node initial-loc initial-loc)
                   (find-score-node initial-loc (zip/down initial-loc))))
  ([initial-loc loc]
   (swap! scoreatom inc)
   (cond
     (nil? loc) nil
     (equal-matrix initial-loc loc) (collect-min-max loc)
     (nil? (zip/up loc)) loc ;WE AT THE TOP!!
     (not (zip/branch? loc)) (recur initial-loc (gimme-new-loc loc))
     (all-set? (extract-children loc)) (let [siblings (zip/rights loc)
                                             newnode (collect-min-max loc)]
                                         (if (zero? (count siblings)) (recur initial-loc (zip/up newnode))
                                           (recur initial-loc (zip/right newnode))))
     :else 
     (recur initial-loc (zip/down loc)))))

(defn find-score 
  "Find Score for any particular board state"
  [board]
  (find-score-node (mk-zip (create-node board))))

(defn make-minmax-keys 
  "Makes a map with a key of possible move and its associated score"
  [node]
  {(:matrixpoint node) (:minmax node)})

(defn get-best-move
  "Get Best move on board"
  [board]
  (let [zipboard (mk-zip (create-node board))]
    (if (not (zip/branch? zipboard)) (:matrixpoint (zip/node zipboard)) ;Return Only move left!
      (let [childlocs (map mk-zip (zip/children zipboard))
            scorelocs (map find-score-node childlocs)
            childnodes (map zip/node scorelocs)
            childscores (map make-minmax-keys childnodes)]
        (key (apply max-key val (into {} childscores))))))) ;(get-tree-keys-scores totalmat)))))


