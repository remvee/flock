(ns flock)

(def n-birds 10)
(def max-speed 10)
(def dim 1000)

(defstruct bird :x :y :dx :dy)

(defn move [bird]
  (assoc bird
    :x (+ (:x bird) (:dx bird))
    :y (+ (:y bird) (:dy bird))))

(def running (atom false))

(defn behave [bird]
  (dosync
   (when @running
     (send-off *agent* #'behave))
   (move bird)))

(defn create-bird []
  (agent (struct bird
                 (rand dim) (rand dim)
                 (rand max-speed) (rand max-speed))))

(def birds (map (fn [_] (create-bird)) (range n-birds)))

(defn start []
  (dosync
   (when (not @running)
     (reset! running true)
     (dorun (map #(send-off % behave) birds)))))

(defn stop []
  (reset! running false))