(ns connectfour.models.existrecur
  (:use [connectfour.models.matrix] 
        [connectfour.models.validate]
        [connectfour.models.playbrain])
  (:require [clojure.zip :as zip]))

(comment 

(defn create-tramp-a 
  "Recursive function to create Posible Outcomes"
  ([tree] (if (not (zero? (:score tree))) tree (create-tramp-a tree nil))) ;Initialize
  ([tree acc]
   (let [matrix (:matrix tree) 
         id (:id tree)
         depth (+ 1 (:depth tree))
         avail (find-all-available matrix)
         mark (if (odd? depth) "x" "y")]
     (if (not (zero? (:score tree))) acc ;If I have a score go ahead and stop
       (if (pos? (count avail))  ;If I have children
         (let [kids (map #(create-node (set-val matrix % mark) % id depth) avail)]
           (for [newChild kids]
             (trampoline create-tramp-a newChild (list acc newChild))))
         acc)))))

(defn create-tramp-new
  "Recursive function to create Posible Outcomes"
  ([tree] (if (not (zero? (:score tree))) tree (create-tramp-new tree nil))) ;Initialize
  ([tree acc]
   (let [matrix (:matrix tree)
         id (:id tree)
         depth (+ 1 (:depth tree))
         avail (find-all-available matrix)
         zero-kids (zero? (count avail))
         have-score (not (zero? (:score tree)))
         mark (if (odd? depth) "x" "y")]
     (if (or have-score zero-kids) acc ;If I have a score or no kids go ahead and stop
       (let [kids (map #(create-node (set-val matrix % mark) % id depth) avail)]
         (for [newChild kids]
           (trampoline create-tramp-new newChild (list acc newChild))))))))
(defn create-tree [matrix]
  (create-tramp-a (create-node matrix) (create-node matrix)))

(defn find-depth [singletree depth]
  (filter #(= (:depth %) depth) (map #(select-keys % [:score :matrixpoint :depth]) (flatten singletree))))
;NEW RECURSIVE SECTION
(defn flatten-tree [tree]
  "helper method"
  (map #(remove nil? %) (map flatten tree)))

(defn get-root-node [tree]
  "Helper Method"
  (first tree))

(defn find-kids
  [parent tree]
  (let [id (:id parent)]
    (filter #(= (:parent %) id) tree)))

(defn find-score
  [parent tree]
  (let [node parent
        totalscore (:score node)
        kids (find-kids node tree)
        no-kids (zero? (count (find-kids node tree)))
        has-score (not (zero? (:score node)))]
    (if (or no-kids has-score) totalscore
      (get-minmax-score (map #(trampoline find-score % tree) kids) (:depth node)))))

;New Recursive Section END

;Deprecated
(defn get-tree-minmax [singletree]
  (loop [depth 0]
    (let [tree-values (map :score (find-depth singletree depth)) ;I just need score out of these maps
          score (get-minmax-score tree-values depth)]
      (if (not (zero? score)) score ;Found Score!!
        (recur (inc depth))))))

;Deprecated Method
(defn get-tree-score-ANTIQUATED [allmat]
  (let [flatmat (flatten allmat)
        results (map #(:score %) flatmat)]
    (remove zero? (remove nil? results))))

(defn get-tree-scores [tree]
  "Get scores for all possible moves on board"
  (let [flattree (flatten-tree tree)]
    ( map #(find-score (get-root-node %) %) flattree)))

(defn get-tree-key [singlemat]
  (let [flatmat (flatten singlemat)]
    (first (distinct (remove nil? (map #(when (= 0 (:depth %)) (:matrixpoint %)) flatmat))))))

(defn get-tree-keys [totalmat]
  "Get Keys for all possible moves on board"
  (vec (map get-tree-key totalmat)))

(defn get-tree-keys-scores [totalmat]
  "Get Tree keys with scores"
  (zipmap (get-tree-keys totalmat) (get-tree-scores totalmat)))

;THis is definitely Deprecated
(defn length-x
  "Calculate the length of a collection or sequence"
  ([coll]
   (length-x coll 0))
  ([coll accumulator]
   (if-let [[x & xs] (seq coll)]
     (recur xs (inc accumulator))
     accumulator)))

;I believe this is Deprecated
(defn first-level-nodes [tree mark depth]
  (let [matrix (:matrix tree)
        id (:id tree)]
    (map #(create-node (set-val matrix % mark) % id depth) (find-all-available matrix))))

;I believe this is Deprecated
(defn first-level-tree [matrix mark parent depth ]
  (map #(create-node (set-val matrix % mark) %) (find-all-available matrix)))

;I believe this is Deprecated
(defn second-level-tree [tree mark]
  (let [matrix (:matrix tree)
        [x y] (:matrixpoint tree)
        parent (:id tree)
        depth (inc (:depth tree))]
    (map #(create-node (set-val matrix % mark) % parent depth) (find-all-available matrix))))
;  (fn [matrix]
;    (case (s
)
