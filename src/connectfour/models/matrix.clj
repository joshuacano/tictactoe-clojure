(ns connectfour.models.matrix)

(def playmatrix (atom [{:spot1 "x" :spot2 0 :spot3 0} {:spot1 0 :spot2 0 :spot3 0} {:spot1 0 :spot2 0 :spot3 0}]))

(defn keyify [x]
  (keyword (str "spot" (+ x 1))))

(defn get-keys [size]
  (vec (map #(keyify %) (range size))))

(defn return-row [size]
  (zipmap (get-keys size) (repeat size 0)))

(defn get-matrix [size]
  (vec (repeat size (return-row size))))

(defn init-matrix [size]
  (if (> size 2)
    (get-matrix size)))

(defn get-val [matrix [x y]]
  ((get matrix y) (keyify x)))

(defn taken [matrix [x y]]
  (not (= (get-val matrix [x y]) 0)))

(defn set-val [matrix [x y] xo]
  "Method to set val in matrix"
  (assoc-in matrix [y (keyify x)] xo))

(defn set-permanent-val [[x y] xo]
  (if (not (taken @playmatrix [x y])) 
    (do 
      (swap! playmatrix assoc-in [y (keyify x)] xo)
      true)
    false))

(defn init-permanent-matrix [size]
  (if (> size 2)
    (reset! playmatrix (get-matrix size))))

(defn find-all-available [matrix]
  (let [size (count matrix)]
    (for [x (range size) 
          y (range size)
          :when (= 0 (get-val matrix [x y]))]
      [x y])))

(defn get-game-board []
  @playmatrix)

(defn count-val [matrix mark]
  (filter #(= mark %) (flatten (map vals matrix))))

  (defn your-turn? [matrix mark]
    "Function to see if it is your turn. Still under construction"
   (let [xcount (count-val matrix "x")
         ycount (count-val matrix "y")]
     (if (and (= mark "y") (< ycount xcount)) true false)))
