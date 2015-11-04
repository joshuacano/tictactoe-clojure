(ns connectfour.routes.home
 (:require [connectfour.layout :as layout]
  [connectfour.models.playbrain :refer :all]
  [connectfour.models.minmax :as mm]
  [connectfour.models.alphabeta :as ab]
  [connectfour.models.matrix :as mat]
  [connectfour.models.validate :refer :all]
  [compojure.core :refer [defroutes GET POST]]
  [ring.util.http-response :refer [ok]]
  [taoensso.timbre :refer [trace debug info warn error fatal]]
  [clojure.java.io :as io]))

(defn home-page []
 (layout/render
  "home.html" {:docs (-> "docs/docs.md" io/resource slurp)}))

(defn about-page []
 (layout/render "about.html"))

(defn return-json [returnVal]
"Helper function to return json"
 {:status 200 
 :headers {"Content-Type" "application/json"} 
 :body returnVal})

(defn init-page []
"Initialize game board"
 (info "Initialize Game Board")
 (let [rn (mat/init-permanent-matrix 3)]
  (return-json (mat/get-game-board))))

(defn matrix-page 
"Page that returns current state of matrix and winner"
[]
(let [matrix (mat/get-game-board)
      won (if (did-somebody-win? matrix) (who-won? matrix) nil)]
 (return-json {:winner won :matrix matrix}))) 

(defn set-and-return-computer-move 
 "Set user move and return computer move"
 [xindex yindex]
 (let [result {:winner nil :x nil :y nil}]
  (do
   (mat/set-permanent-val [xindex yindex] "x")
   (if (did-somebody-win? (mat/get-game-board)) (assoc result :winner (who-won? (mat/get-game-board)))
     (let [best-move (ab/get-best-move (mat/get-game-board))
      saved (mat/set-permanent-val best-move "y")
      won (if (did-somebody-win? (mat/get-game-board)) (who-won? (mat/get-game-board)) nil)]
      (assoc result :winner won :x (first best-move) :y (second best-move)))))))

(defn handle-move 
 "Run user and computer move and return appropriate move to front-end"
 [xindex yindex]
 (let [response (set-and-return-computer-move xindex yindex)
       winner (if (string? response) response nil)
       x (when (nil? winner) (first response))
       y (when (nil? winner) (second response))]
    {:winner winner :x x :y y}))

(defroutes home-routes
 (GET "/" [] (home-page))
 (GET "/matrix" [] (matrix-page))
 (GET "/init" [] (init-page))
 (GET "/about" [] (about-page))
 (POST "/store-move" request
  (let [{:keys [x y]} (:params request)]
   (return-json (set-and-return-computer-move x y)))))
