(ns connectfour.models.playbrain
  (:use [connectfour.models.matrix] 
        [connectfour.models.validate]) )

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

(defn get-minmax-score [coll depth]
  "Get MinMax algorithm result for a collection of integers"
  (if (even? depth) (apply max coll) (apply min coll)))

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

(defn get-tree-scores [totalmat]
  "Get scores for all possible moves on board"
  (vec (map get-tree-minmax totalmat)))

(defn get-tree-key [singlemat]
  (let [flatmat (flatten singlemat)]
    (first (distinct (remove nil? (map #(when (= 0 (:depth %)) (:matrixpoint %)) flatmat))))))

(defn get-tree-keys [totalmat]
  "Get Keys for all possible moves on board"
  (vec (map get-tree-key totalmat)))

(defn get-tree-keys-scores [totalmat] 
  "Get Tree keys with scores"
   (zipmap (get-tree-keys totalmat) (get-tree-scores totalmat)))

(defn get-random-move [matrix]
  "Get a Random move on Board"
  (let [available (find-all-available matrix)
        size (count available)]
    (nth available (rand-int size))))

(defn play-random-move [matrix mark]
  (set-permanent-val (get-random-move matrix) mark))

(defn get-best-move [board]
  "Get Best move on board"
  (let [totalmat (create-tramp-new (create-node board))]
    (key (apply max-key val (get-tree-keys-scores totalmat)))))

(defn play-best-move [board mark]
  "Set Best move on board"
  (set-permanent-val (get-best-move board) mark))

(defn play-computer-v-computer [] 
  "Play computer v Computer till someone wins!" 
  (do 
    (init-permanent-matrix 3)
    (loop [counter 1
           any-winners? (did-somebody-win? (get-game-board))] 
            (println "board is " (str (get-game-board)))
      (if any-winners? (who-won (get-game-board)) ;If somebody win, call the who-won function and exit!
          (do 
            "This do loop is strictly for Debugging"
            (when (and (> counter 2) (even? counter)) 
              (let [totalmat (create-tramp-new (create-node (get-game-board)))] 
                (println "Here are the possible Moves " (str (get-tree-keys-scores totalmat)))))
        (if (odd? counter) 
         (do 
            (println "Playing random, Move Count is " (str counter))
            (play-random-move (get-game-board) "x"))
         (do
            (println "Playing Best Move, Move Count is " (str counter))
            (play-best-move (get-game-board) "y")))
            (recur (inc counter) (did-somebody-win? (get-game-board))))))))

(defn flatten-tree [tree]
  (map #(remove nil? %) (map flatten tree)))

(defn get-root-nodes [tree]
  (map first (flatten-tree tree)))

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
         (get-minmax-score (map #(find-score % tree) kids) (:depth node)))))

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
