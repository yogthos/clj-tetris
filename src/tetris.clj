(ns tetris
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Font Graphics Color Toolkit)   
    (java.awt.event ActionListener KeyListener KeyEvent)))
 
(def COLS 10)
(def ROWS 20)
(def WIDTH 300)
(def HEIGHT 600)
(def OFFSET (atom [0 0]))
(def ROTATION (atom nil))
(def COLORS [Color/red Color/blue Color/green Color/yellow
             Color/yellow Color/orange Color/magenta])
(def SHAPES  [[[0 1] [0 2] [0 3] [0 4]]
                [[0 0] [0 1] [1 1] [1 2]]
                [[1 2] [1 1] [0 1] [0 0]]
                [[0 1] [1 1] [1 0] [2 1]]
                [[0 0] [0 1] [1 0] [1 1]]
                [[0 0] [0 1] [0 2] [1 2]]
                [[1 0] [1 1] [1 2] [0 2]]])

(defn get-block []
  (let [shape (rand-nth SHAPES)
        offset (inc (rand-int (- COLS 3)))]
    {:color (rand-nth COLORS)
     :shape (map (fn [[x y]] [(+ x offset) y]) shape)}))

(defn get-board []
  (vec (take (* ROWS COLS) (repeat Color/black))))

(defn pos-to-xy [pos]
  (let [x (mod pos COLS)
        y (int (/ (- pos x) COLS))]
    [x, y]))

(defn collides?
  ([board x y pos]
    (let [[posx posy] (pos-to-xy pos)]    
      (and
        (> x (- 1))
        (< x COLS)
        (< y ROWS)
        (not (and
               (= posx x)
               (= posy y)                       
               (not= Color/black (get board (+ pos COLS))))))))
  ([board shape pos]
    (every?
      #{true}
      (for [[x y] shape]
        (collides? board x y pos))))
  ([board shape]
    (not (reduce
           #(and %1 (collides? board shape %2))
           (range (count board)))) ))

(defn rotate [board shape]
  (if (nil? @ROTATION)
    shape
    (let [[avg-x avg-y] (->> shape
                          (reduce
                            (fn [[tx ty] [x y]]
                              [(+ tx x) (+ ty y)]))
                          (map #(int (/ % 4))))
          
          rotated (map (fn [[x y]]
                         [(int (+ avg-x (- y avg-y)))
                          (int (- avg-y (- x avg-x)))])
                       shape)]     
      (if (collides? board rotated)
        shape rotated))))

(defn shift [board shape]
  (let [shifted (map
                  (fn [[x y]]
                    [(+ x (first @OFFSET)) y])
                  shape)]
    (if (collides? board shifted)
      shape shifted)))

(defn transform [board {:keys [color shape]} drop?]
  (let [rotated (->> shape (shift board) (rotate board))]
    {:color color
     :shape (if drop?
              (map (fn [[x y]] [x (if drop? (inc y) y)])
                   rotated)
              rotated)}))

(defn clear-lines [board]
  (let [new-board (->> board
                    (partition COLS)
                    (filter #(some #{Color/black} %))
                    (apply concat))         
        num-removed (- (count board) (count new-board))]   
    [num-removed
     (into (vec (take num-removed (repeat Color/black)))       
       new-board)]))

(defn update-board [board {:keys [color shape]}]
  (vec (map #(let [[x y] (pos-to-xy %)]
               (if (some (fn [[px py]] (and (= x px) (= y py)))
                     shape)
                 color (get board %)))
           (range (count board)))))

(defn game-over? [board]
  (not (reduce  #(and %1 (= Color/black %2))
         (butlast (rest (take COLS board))))))

;;;;;;Controls;;;;
(defn handle-input [#^KeyEvent event]
  (condp = (.getKeyCode event)
    KeyEvent/VK_LEFT  (swap! OFFSET #(map + [-1 0] %))
    KeyEvent/VK_RIGHT (swap! OFFSET #(map + [1 0] %))
    KeyEvent/VK_UP (reset! ROTATION :left)
    KeyEvent/VK_DOWN (reset! ROTATION :right)))

(defn input-listener []
  (proxy [ActionListener KeyListener] []
    (actionPerformed [e])
    (keyPressed [e] (handle-input e))
    (keyReleased [e])
    (keyTyped [e])))

;;;;;;;UI;;;;;;;;;
(defn draw [#^Canvas canvas draw-fn]
  (let [buffer  (.getBufferStrategy canvas)
        g       (.getDrawGraphics buffer)]
    (try
      (draw-fn g)
      
      (finally (.dispose g)))
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn draw-square [x y color #^Graphics g]
  (let [width  (/ WIDTH COLS)
        height (/ HEIGHT ROWS)
        xpos   (* x width)
        ypos   (* y width)]
    (doto g
      (.setColor color)
      (.fillRect xpos ypos width height)
      (.setColor Color/black)
      (.drawRect xpos ypos width height))))

(defn draw-text [#^Graphics g color text x y]
  (doto g
    (.setColor color)
    (.drawString text x y)))

(defn draw-game-over [score]
  (fn [#^Graphics g] 
    (doto g
      (.setColor (new Color (float 0) (float 0) (float 0) (float 0.7)))
      (.fillRect 0 0 WIDTH HEIGHT))
    (draw-text g Color/red "GAME OVER" (- (/ WIDTH 2) 50) (/ HEIGHT 2))
    (draw-text g Color/red (str "Final Score: " score) (- (/ WIDTH 2) 55) (+ 15 (/ HEIGHT 2)))))

(defn draw-board [board block score]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/BLACK)
      (.fillRect 0 0 WIDTH HEIGHT))
    
    (doseq [square (range (count board))]
      (let [[x y] (pos-to-xy square)]
        (draw-square x y (get board square) g)))
    
    (doseq [[x y] (:shape block)]
      (draw-square x y (:color block) g))
    
    (draw-text g Color/green (str "score: " score) 20 25)))

(defn -main [& args]
  (let [frame  (JFrame. "Tetris")
        canvas (Canvas.)]
    (doto frame
      (.setSize WIDTH (+ (/ HEIGHT ROWS) HEIGHT))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))
    
    (doto canvas
      (.createBufferStrategy 2)
      (.addKeyListener (input-listener))
      (.setVisible true)
      (.requestFocus))
    
    ;;game loop
    (loop [score    0
           board    (get-board)
           block    (get-block)
           old-time (System/currentTimeMillis)]
      
      (reset! OFFSET [0 0])
      (reset! ROTATION nil)
      (Thread/sleep 10)
      (draw canvas (draw-board board block score))      
      
      (let [cur-time (System/currentTimeMillis)
            new-time (long (if (> (- cur-time old-time) 250)
                             cur-time
                             old-time))
            drop?    (> new-time old-time)
            [num-removed new-board] (clear-lines board)]
        (cond
          (game-over? board)
          (draw canvas (draw-game-over score))
          
          (collides? board (:shape block))
          (recur
            score
            (update-board board block)
            (get-block)
            new-time)
          
          :default
          (recur
            (+ score (* num-removed num-removed))
            new-board
            (transform board block drop?)
            new-time))))))

;(-main)