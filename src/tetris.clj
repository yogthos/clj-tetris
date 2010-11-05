(ns tetris
  (:import
    (javax.swing JFrame)
    (java.awt Canvas Graphics Color Toolkit)    
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:gen-class))

(def *cols* 10)
(def *rows* 20)
(def *width* 300)
(def *height* 600)
(def *offset* (atom [0, 0]))
(def *rotation* (atom nil))
(def *shapes*  [[[0,1],[0,2],[0,3],[0,4]]
                [[0,0],[0,1],[1,1],[1,2]]
                [[1,2],[1,1],[0,1],[0,0]]
                [[0,0],[0,1],[1,0],[1,1]]
                [[0,0],[0,1],[0,2],[1,2]]
                [[1,0],[1,1],[1,2],[0,2]]])

(defn get-shape []
  (let [shape (rand-nth *shapes*)
        offset (inc (rand-int (- *cols* 3)))]
    (map (fn [[x y]] [(+ x offset), y]) shape)))

(defn get-board []
  (vec (range (* *rows* *cols*))))

(defn pos-to-xy [pos]
  (let [x (mod pos *cols*)
        y (int (/ (- pos x) *cols*))]
    [x, y]))

(defn collides?
  ([board x y pos]
    (let [[posx posy] (pos-to-xy pos)]     
      (and 
        (> x (- 1)) 
        (< x *cols*)
        (< y *rows*)
        (not (and 
              (= posx x)
              (= posy y)                        
              (not (get board (+ pos *cols*))))))))
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
  (if (nil? @*rotation*)
    shape
    (let [[avg-x avg-y] (->> shape
                          (reduce 
                            (fn [[tx ty] [x y]]
                              [(+ tx x), (+ ty y)]))
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
                    [(+ x (first @*offset*)), y])
                  shape)]
    (if (collides? board shifted)
      shape shifted)))

(defn transform [board shape drop?]
  (let [shifted (shift board shape)
        rotated (rotate board shifted)]
    (if drop?
      (map (fn [[x y]]
             [x, (if drop? (inc y) y)]) rotated)
      rotated)))

(defn clear-lines [board]
  (let [new-board (apply concat 
                    (filter #(some #{true} %) 
                      (partition *cols* board)))]    
    (into  
      (vec (map (fn [_] true) 
             (range (- (count board) (count new-board)))))
      new-board)))

(defn update-board [board shape]
  (vec (map #(let [[x y] (pos-to-xy %)]
               (if (some 
                     (fn [[px py]] (and(= x px) (= y py)))
                     shape)
                 false (get board %)))
         (range (count board)))))

(defn game-over? [board]
  (not (reduce #(and %1 %2)
         (butlast (rest (take *cols* board))))))

;;;;;;Controls;;;;
(def *dirs* {KeyEvent/VK_LEFT  [-1, 0]
             KeyEvent/VK_RIGHT [1, 0]
             KeyEvent/VK_UP :left
             KeyEvent/VK_DOWN :right})

(defn handle-input [#^KeyEvent event]
  (let [key (.getKeyCode event)]
    (cond
      (or (= key KeyEvent/VK_LEFT) (= key KeyEvent/VK_RIGHT))
      (let [disp (*dirs* key)]
        (when disp (swap! *offset* #(map + disp %))))
      (or (= key KeyEvent/VK_UP) (= key KeyEvent/VK_DOWN))
      (reset! *rotation* (*dirs* key)))))

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

(defn draw-square [x y #^Graphics g]
  (let [width  (/ *width* *cols*)
        height (/ *height* *rows*)
        xpos   (* x width)
        ypos   (* y width)]
    (doto g
      (.setColor Color/RED)
      (.fillRect xpos, ypos, width, height)
      )))

(defn draw-game-over [#^Graphics g]
  (doto g
   (.setColor Color/BLACK)
   (.fillRect 0 0 *width* *height*)
   (.setColor Color/green)
   (.drawString "GAME OVER" (- (/ *width* 2) 50)  (/ *height* 2))))

(defn draw-board [board shape]
  (fn [#^Graphics g]
    (doto g
      (.setColor Color/BLACK)
      (.fillRect 0 0 *width* *height*))
    
    (doseq [square (range (count board))]
      (when (not (get board square))
       (let [[x y] (pos-to-xy square)]
         (draw-square x y g))))
    
    (doseq [[x y] shape]
      (draw-square x y g))))

(defn -main [& args]
  (let [frame  (JFrame. "Tetris")
        canvas (Canvas.)]
    (doto frame
      (.setSize *width* (+ (/ *height* *rows*) *height*))
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
    (loop [board    (get-board)
           shape    (get-shape)
           old-time (System/currentTimeMillis)]
      
      (reset! *offset* [0,0])
      (reset! *rotation* nil)
      (Thread/sleep 10)
      (draw canvas (draw-board board shape))
      
      (let [cur-time (System/currentTimeMillis)
            new-time (long (if (> (- cur-time old-time) 150)
                             cur-time
                             old-time))
            drop?    (> new-time old-time)]
        (cond
          (game-over? board)
          (draw canvas draw-game-over)
          
          (collides? board shape)
          (recur (update-board board shape)
            (get-shape)
          new-time)
          
          :default
          (recur 
            (clear-lines board)
            (transform board shape drop?)
            new-time)
          )))))
(-main)