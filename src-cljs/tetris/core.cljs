(ns tetris.core  
  (:require [tetris.game :as game]))

(defn log [& items]
  (.log js/console (apply str (interpose ", " items))))

;;Controls
(defn ^:export keyDown [evt]
  (condp = evt/keyCode
    37 (swap! game/OFFSET #(map + [-1 0] %)) 
    39 (swap! game/OFFSET #(map + [1 0] %))
    38 (reset! game/ROTATION :left)
    40 (reset! game/ROTATION :right))
  (log (str "game/OFFSET: " @game/OFFSET "\ngame/ROTATION: " @game/ROTATION)))

;;UI
(defn clear [ctx]
  (set! (.-fillStyle ctx) "black")
  (.fillRect ctx 0 0 @game/WIDTH @game/HEIGHT))

(defn draw-square [ctx color x y]  
  (let [width  (/ @game/WIDTH game/COLS)
        height (/ @game/HEIGHT game/ROWS)
        xpos   (* x width)
        ypos   (* y width)]
        
    (set! (.-fillStyle ctx) color)    
    (.fillRect ctx xpos ypos width height)
    (set! (.-fillStyle ctx) "black")
    (.strokeRect ctx xpos ypos width height)))

(defn draw-text [ctx color text x y]
  (set! (.-fillStyle ctx) color)
  (set! (.-font ctx) "20px Verdana")
  (.fillText ctx text x y))

(defn draw-game-over [ctx score]
    (set! (.-fillStyle ctx) "rgba(255, 255, 255, 0.5)")
    (.fillRect ctx 0 0 @game/WIDTH @game/HEIGHT)
    (draw-text ctx "red" "GAME OVER" (- (/ @game/WIDTH 2) 50) (/ @game/HEIGHT 2))
    (draw-text ctx "red" (str "Final Score: " score) (- (/ @game/WIDTH 2) 55) (+ 15 (/ @game/HEIGHT 2))))


(defn draw-board [ctx board block score]     
  (clear ctx)
  
  ;render the board
  (doseq [square (range (count board))]
    (let [[x y] (game/pos-to-xy square)]
      (draw-square ctx (get board square) x y)))
  
  ;draw the current block
  (doseq [[x y] (:shape block)]
    (draw-square ctx (:color block) x y))
  
  (draw-text ctx "green" (str "score:" score)  20 25))

(declare game-loop)
(defn game-loop [ctx score board block old-time]       
  (reset! game/OFFSET [0 0])
  (reset! game/ROTATION nil)
  
  (draw-board ctx board block score)
  
  (let [cur-time (.getTime (new js/Date))
        new-time (if (> (- cur-time old-time) 250)
                   cur-time
                   old-time)
        drop? (> new-time old-time)
        [num-removed new-board] (game/clear-lines board)]
    
    (cond
      (game/game-over? board)
      (draw-game-over ctx score)
      
      (game/collides? board (:shape block))
      (js/setTimeout 
        (fn []  
          (game-loop ctx
                     score 
                     (game/update-board board block) 
                     (game/get-block) 
                     new-time))
        5)
            
      :default
      (js/setTimeout 
         (fn [] 
           (game-loop ctx
                      (+ score (* num-removed num-removed)) 
                      new-board 
                      (game/transform board block drop?) 
                      new-time))
         5))))

(defn ^:export init []    
  (let [canvas (.getElementById js/document "canvas")
        ctx (.getContext canvas "2d")]
    
    (reset! game/WIDTH (.-width canvas))
    (reset! game/HEIGHT (.-height canvas))
    
    (.addEventListener js/window "keydown" keyDown true)      
    (game-loop ctx 0 (game/get-board) (game/get-block) (.getTime (new js/Date)))))
