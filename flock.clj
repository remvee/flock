(ns flock)

(def n-birds 10)
(def max-speed 10)
(def drunkness 3)
(def leader-attraction 1.2)
(def privacy-radius 150)

(def dim 1250)
(def behave-sleep-ms 20)

(defstruct bird :x :y :dx :dy)

(def running (atom false))

(defn create-bird []
  (agent (struct bird
                 (rand dim) (rand dim)
                 (- (rand max-speed) (rand max-speed))
                 (- (rand max-speed) (rand max-speed)))))

(def birds (map (fn [_] (create-bird)) (range n-birds)))


;;;;;;;;;;;;;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;

(defn distance [x y]
  "Distance between two birds."
  (Math/sqrt (+ (* x x) (* y y))))

(defn neighbors-sorted-by-distance [birds bird]
  "Return list of birds sorted by distance from given birds."
  (sort-by #(distance (- (:x bird) (:x %))
                      (- (:y bird) (:y %))) birds))

;;;;;;;;;;;;;;; BEHAVIOUR ;;;;;;;;;;;;;;;;;;;;;;;

(defn move [bird]
  (assoc bird
    :x (+ (:x bird) (:dx bird))
    :y (+ (:y bird) (:dy bird))))

(defn bounce-world [bird]
  (assoc bird
    :dx (cond (> (:x bird) dim) (* -1 (Math/abs (:dx bird)))
              (neg? (:x bird))  (Math/abs (:dx bird))
              :otherwise        (:dx bird))
    :dy (cond (> (:y bird) dim) (* -1 (Math/abs (:dy bird)))
              (neg? (:y bird))  (Math/abs (:dy bird))
              :otherwise        (:dy bird))))

(defn bounce-others [bird]
  (let [neighbor (nth (neighbors-sorted-by-distance (map deref birds) bird) 1)
        distance (distance (- (:x bird) (:x neighbor))
                           (- (:y bird) (:y neighbor)))]
    (if (< distance privacy-radius)
      (let [dx       (- (:x neighbor) (:x bird))
            dy       (- (:y neighbor) (:y bird))]
        (assoc bird
          :dx (- (:dx bird) (/ dx distance))
          :dy (- (:dy bird) (/ dy distance))))
      bird)))

(defn follow-leader [bird]
  (let [leader (last (neighbors-sorted-by-distance (map deref birds) bird))
        dx     (- (:x leader) (:x bird))
        dy     (- (:y leader) (:y bird))
        n      (max (Math/abs dx) (Math/abs dy))]
    (assoc bird
      :dx (+ (:dx bird) (* leader-attraction (/ dx n)))
      :dy (+ (:dy bird) (* leader-attraction (/ dy n))))))

(defn cap-speed [bird]
  (let [speed (distance (:dx bird) (:dy bird))]
    (if (> speed max-speed)
      (let [f (/ max-speed speed)]
        (assoc bird
          :dx (* (:dx bird) f)
          :dy (* (:dy bird) f)))
      bird)))

(defn stumble [bird]
  (assoc bird
    :dx (+ (:dx bird) (- (/ drunkness 2) (rand drunkness)))
    :dy (+ (:dy bird) (- (/ drunkness 2) (rand drunkness)))))

(defn behave [bird]
  (dosync
   (when @running
     (. Thread (sleep behave-sleep-ms))
     (send-off *agent* #'behave))
   (-> bird
       follow-leader
       bounce-others
       stumble
       cap-speed
       bounce-world
       move)))


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
        bg  (.getGraphics img)]
    (doto bg
      (.setColor Color/white)
      (.fillRect 0 0 (.getWidth img) (.getHeight img)))
    (doseq [bird (map deref birds)]
      (render-bird bg bird))
    (.drawImage g img 0 0 nil)
    (.dispose bg)))

(def panel (doto (proxy [JPanel] [] (paint [g] (render g)))
             (.setPreferredSize (Dimension. (* scale dim) (* scale dim)))))

(def frame (doto (new JFrame) (.add panel) .pack .show))

(defn animate [x]
  (when @running
    (. panel (repaint))
    (. Thread (sleep animation-sleep-ms))
    (send-off *agent* #'animate)
    nil))


;;;;;;;;;;;;;;; CONTROLS ;;;;;;;;;;;;;;;;;;;;;;;

(defn start []
  (dosync
   (when (not @running)
     (reset! running true)
     (send-off (agent nil) animate)
     (dorun (map #(send-off % behave) birds)))))

(defn stop []
  (reset! running false))

; (start)
; (stop)