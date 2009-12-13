(ns flock)

(def n-birds 10)
(def max-speed 10)
(def dim 1250)
(def behave-sleep-ms 20)

(defstruct bird :x :y :dx :dy)

(defn move [bird]
  (assoc bird
    :x (+ (:x bird) (:dx bird))
    :y (+ (:y bird) (:dy bird))))

(defn bounce [bird]
  (assoc bird
    :dx (cond (> (:x bird) dim) (* -1 (Math/abs (:dx bird)))
             (neg? (:x bird))  (Math/abs (:dx bird))
             :otherwise (:dx bird))
    :dy (cond (> (:y bird) dim) (* -1 (Math/abs (:dy bird)))
             (neg? (:y bird))  (Math/abs (:dy bird))
             :otherwise (:dy bird))))
    
(def running (atom false))

(defn behave [bird]
  (dosync
   (when @running
     (. Thread (sleep behave-sleep-ms))
     (send-off *agent* #'behave))
   (-> bird bounce move)))

(defn create-bird []
  (agent (struct bird
                 (rand dim) (rand dim)
                 (- (rand max-speed) (rand max-speed))
                 (- (rand max-speed) (rand max-speed)))))

(def birds (map (fn [_] (create-bird)) (range n-birds)))


;;;;;;;;;;;;;;; RENDERING ;;;;;;;;;;;;;;;;;;;;;;;

(def scale 0.5)
(def animation-sleep-ms 50)

(import 
 '(java.awt Color Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))

(defn render-bird [g bird]
  (let [x (* scale (:x bird))
        y (* scale (:y bird))]
    (doto g
      (.setColor Color/black)
      (.fillRect x y 5 5))))

(defn render [g]
  (let [img (BufferedImage. (* scale dim) (* scale dim) BufferedImage/TYPE_INT_RGB)
        bg (.getGraphics img)]
    (doto bg
      (.setColor Color/white)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (doseq [bird (map deref birds)]
      (render-bird bg bird))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def panel (doto (proxy [JPanel] [] (paint [g] (render g)))
             (.setPreferredSize (Dimension. (* scale dim) (* scale dim)))))

(def frame (doto (new JFrame) 
             (.add panel)
             .pack
             .show))

(defn animate [x]
  (. panel (repaint))
  (. Thread (sleep animation-sleep-ms))
  (send-off *agent* #'animate)
  nil)


;;;;;;;;;;;;;;; CONTROLS ;;;;;;;;;;;;;;;;;;;;;;;

(defn start []
  (dosync
   (when (not @running)
     (reset! running true)
     (send-off (agent nil) animate)
     (dorun (map #(send-off % behave) birds)))))

(defn stop []
  (reset! running false))
