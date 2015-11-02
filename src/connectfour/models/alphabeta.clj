(ns connectfour.models.alphabeta 
  "code for Alphabeta search"
  (:use [connectfour.models.matrix] 
        [connectfour.models.validate])
  (:require [clojure.zip :as zip] 
            [connectfour.models.minmax :as mm])
  (:gen-class))

(defn node-tic [node children]
  "Make a new node for alpha beta algorithm in tic tac toe" 
  (cond
    (nil? node) nil
    (map? node) (assoc node :inputs (map #(hash-map :parent (:parent %) :depth (:depth %) :matrix (:matrix %) :score (:score %) :minmax (:minmax %) :alpha (:alpha %) :beta (:beta %) :matrixpoint (:matrixpoint %)) children))
    (vector? node) (into [] children)
    :else node))

(defn children-tic 
  "Make children for tic tac toe"
  [node]
  (let [matrix (:matrix node)
        id (:id node)
        depth (+ 1 (:depth node))
        avail (find-all-available matrix)
        already-kids? (not (nil? (:inputs node)))
        mark (if (odd? depth) "x" "y")
        children (if already-kids? 
                   (:inputs node)
                   (map #(mm/create-node (set-val matrix % mark) % id depth) avail))]
    (sort-by #(Math/abs (:score %)) children)))


(defn mk-zip [root]
  "Master function to create board"
    (zip/zipper mm/branch-tic children-tic node-tic root))

(defn is-minimum-node [node] 
  "Check if this node is a minimizer"
  (let [avail (count-available (:matrix node))]
    (odd? avail)))

(defn get-alpha [node]
  "Get Alpha value for node Returns NEGATIVE_INFINITY if not found"
    (if (nil? (:alpha node)) Double/NEGATIVE_INFINITY (:alpha node)))

(defn get-beta [node]
  "Get Beta Value for node returns POSITIVE_INFINITY if not found"
    (if (nil? (:beta node)) Double/POSITIVE_INFINITY (:beta node)))

(defn check-alpha-beta? 
  "Checks if this higher node can be overwritten with relevant alpha/beta info"
  [node lownode]
      (or 
        (< (get-alpha node) (get-alpha lownode) (get-beta node))
        (< (get-alpha node) (get-beta lownode) (get-beta node)))
      )

(defn set-alpha-beta
  "Set alpha beta node in Tree, used by set-alpha-beta-up and set-alpha-beta-down"
  ([node] ;This should only be called at beginning I suppose?
  (let [minNode (is-minimum-node node)]
    (assoc node (if minNode :beta :alpha) (:score node))))
  ([node lownode] 
  (let [minNode (is-minimum-node node)
        overwrite? (check-alpha-beta? node lownode)
        value (if minNode (:alpha lownode) (:beta lownode))]
    (assoc node (if minNode :beta :alpha) value))))

(defn set-alpha-beta-loc 
  "Set's alpha beta for a loc rather than a node"
  [loc]
  (zip/edit loc set-alpha-beta))

(defn terminalNode? 
  "Checks if this node is a leaf node essentially"
  [node]
  (or (zero? (count-available (:matrix node)))
      (not (zero? (:score node)))))

(defn set-alpha-beta-down 
  "Pass Alpha beta down the tree"
  [node highNode]
  (let [newNode (assoc node :alpha (get-alpha highNode) :beta (get-beta highNode))
        minNode (is-minimum-node newNode)
        terminalNode (terminalNode? newNode)]
      (cond
        (not (terminalNode? newNode)) newNode
        (< (get-alpha newNode) (:score newNode) (get-beta newNode))  (if minNode
                                                             (assoc newNode :beta (:score newNode))
                                                             (assoc newNode :alpha (:score newNode)))
        :else newNode)))

(defn pass-alpha-beta-down
  "Pass alpha beta loc down the tree"
  [highLoc loc] 
   (let [highNode (zip/node highLoc)]
     (zip/edit loc set-alpha-beta-down highNode)))

(defn continue-search? 
  "Returns false if the this section of the tree can be pruned!"
  [newParent] ;Stop Looking if not feasible!
  (let [node (zip/node newParent)
        alpha (get-alpha node)
        beta (get-beta node)]
    (< alpha beta)))

(defn is-ab-set? 
  "Checks if Alpha value if node is a maximizer, else checks if Beta value is set if node is a minimizer"
  [value keyname]
  (if (nil? value) false 
  (if (= keyname :alpha) (> value Double/NEGATIVE_INFINITY) 
    (< value Double/POSITIVE_INFINITY))))

(defn clean-children 
  "Clean children if not set"
 [children minNode?]
 (if minNode? 
   (remove #(not (is-ab-set? (get-alpha %) :alpha)) children)
   (remove #(not (is-ab-set? (get-beta %) :beta)) children)))
            
(defn set-best-ab-up 
  "Set the best alpha beta possible from kids"
  [loc]
  (let [minNode? (is-minimum-node (zip/node loc))
        cleanedChildren (clean-children (zip/children loc) minNode?)]
    (cond
      (nil? cleanedChildren) loc
      minNode? (zip/edit loc (defn set-p [node children] (assoc node :beta (apply min (map #(get-alpha %) children)))) cleanedChildren)
      :else (zip/edit loc (defn set-p [node children] (assoc node :alpha (apply max (map #(get-beta %) children)))) cleanedChildren))))

(defn all-alpha-beta-set? 
"A method to check if all children in a location have been looked at at least once"
[loc]
  (if (not (zip/branch? loc)) true
  (let [minNode? (is-minimum-node (zip/node loc))
        children (mm/extract-children loc)
        keyname (if minNode? :alpha :beta)]
  (every? #(is-ab-set? (keyname %) keyname) children))))

(defn am-i-ab-set? [node]
"Checks if self is set, to ensure that node has already checked all options"
      (if (is-minimum-node node)
        (is-ab-set? (:beta node) :beta)
        (is-ab-set? (:alpha node) :alpha)))

(def cntatom (atom 0)) ;Counter for Debugging amount of nodes searched by Main Alpha Beta function 

(defn try-alpha-single
  "Main recursive function to search tree using Alpha Beta Pruning"
  [initial-loc move]
  (swap! cntatom inc)
  (cond 
    ;Node is a leaf with no siblings
    (and (not (zip/branch? move))
         (nil? (zip/right move)))   (let [newmove (set-alpha-beta-loc move)]
                                      (recur initial-loc (set-best-ab-up (zip/up newmove))))
    ;Node is a leaf with siblings
    (not (zip/branch? move)) (let [newmove (set-alpha-beta-loc move)
                                   newParent (set-best-ab-up (zip/up newmove))]
                               (recur initial-loc (pass-alpha-beta-down newParent (zip/right newmove))))
    ;Function has visited all children and is finished!
    (and (mm/equal-matrix initial-loc move)
         (all-alpha-beta-set? move)) move
    ;This branch can now be pruned, Pruning step                               
    (not (continue-search? move)) (let [uploc (set-best-ab-up (zip/up move))] 
                                    (recur initial-loc (if (nil? (zip/right move)) uploc
                                                         (pass-alpha-beta-down uploc (zip/right move))))); Prune Section
    ;Cannot prune and is branch, so is deciding if should go down or right
    (not (am-i-ab-set? (zip/node (zip/down move))))  (let [newNode (pass-alpha-beta-down move (zip/down move))]
                                                       (recur initial-loc newNode))
    ;at end of breadth search
    (and (am-i-ab-set? (zip/node move))
         (nil? (zip/right move))) 
    (recur initial-loc (set-best-ab-up (zip/up move)))
    ;Move to sibling node for branch
    :else 
    (let [newParent (set-best-ab-up (zip/up move))]
      (recur initial-loc (pass-alpha-beta-down newParent (zip/right move))))))

(defn convert-children 
  "Convert children to simple keys for playbrain to handle"
  [loc]
  (let [keyname (if (is-minimum-node (zip/node loc)) :alpha :beta)
        newChildren (remove #(not (< (get-alpha %) (get-beta %))) (zip/children loc))]
    (map #(hash-map (:matrixpoint %) (keyname %)) newChildren)))

(defn get-best-key 
  "Get Best move from searched tree"
  [loc]
  (if (is-minimum-node (zip/node loc))
    (key (apply min-key val (into {} (convert-children loc))))
    (key (apply max-key val (into {} (convert-children loc))))))

(defn run-alpha-beta-loc
  "Main alpha beta function, will get alpha beta searched loc for any game board"
  [board]
  (let [startLoc (mk-zip (mm/create-node board))]
    (if (zip/branch? startLoc) 
      (try-alpha-single startLoc (zip/down startLoc))
      startLoc)))

(defn get-best-move 
  "Get best move for any game board using alpha beta pruning"
  [board]
  (get-best-key (run-alpha-beta-loc board)))

(defn start-alpha-bottom 
  "Zipper helper to zip all the way to the bottom, Used for debugging or testing"
  []
   (let [zn (mm/zip-last-node (mk-zip (mm/create-node (get-game-board))))]
     (zip/edit zn set-alpha-beta)))

