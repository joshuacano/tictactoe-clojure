(ns connectfour.models.matrix
  "Matrix declares all of the basic functionality and helper funcions for a tic tac toe game board"
  )

(def playmatrix (atom [{:spot1 "x" :spot2 0 :spot3 0} {:spot1 0 :spot2 0 :spot3 0} {:spot1 0 :spot2 0 :spot3 0}]))

(defn keyify 
  "Keyify the column names for keys of matri"
  [x]
  (keyword (str "spot" (+ x 1))))

(defn get-keys 
  "Get Keys of each spot on a board"
  [size]
  (vec (map #(keyify %) (range size))))

(defn return-row 
  "Return empty row"
  [size]
  (zipmap (get-keys size) (repeat size 0)))

(defn get-matrix 
  "Return square board based on size passed in"
  [size]
  (vec (repeat size (return-row size))))

(defn init-matrix 
  "Initialize matrix"
  [size]
  (if (> size 2)
    (get-matrix size)))

(defn get-val 
  "Get value at particular index on board"
  [matrix [x y]]
  ((get matrix y) (keyify x)))

(defn taken? 
  "Is space taken?"
  [matrix [x y]]
  (not (= (get-val matrix [x y]) 0)))

(defn set-val 
  "Method to set val in matrix"
  [matrix [x y] xo]
  (assoc-in matrix [y (keyify x)] xo))

(defn set-permanent-val 
  "Set Permanent value in Matrix atom for system"
  [[x y] xo]
  (if-not (taken? @playmatrix [x y])
    (do 
      (swap! playmatrix assoc-in [y (keyify x)] xo)
      true)
    false))

(defn init-permanent-matrix 
  "Initialize permanent gameboard"
  [size]
  (when (> size 2)
    (reset! playmatrix (get-matrix size))))

(defn find-all-available 
  "Return all available spaces on board"
  [matrix]
  (let [size (count matrix)]
    (for [x (range size) 
          y (range size)
          :when (= 0 (get-val matrix [x y]))]
      [x y])))

(defn count-available 
  "Count available spaces on board"
  [matrix]
   (count (find-all-available matrix)))

(defn get-game-board 
  "Return board"`
  []
  @playmatrix)

(defn count-val 
  "Count instances of a particular mark on board"
  [matrix mark]
  (filter #(= mark %) (flatten (map vals matrix))))

  (defn your-turn? 
    "Function to see if it is your turn. Still under construction"
    [matrix mark]
   (let [xcount (count-val matrix "x")
         ycount (count-val matrix "y")]
     (if (and (= mark "y") (< ycount xcount)) true false)))

