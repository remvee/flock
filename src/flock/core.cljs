(ns flock.core
  (:require [clojure.string :as s]
            [reagent.core :as reagent :refer [atom]]))

(def n-birds (atom 15))
(def max-speed (atom 15))
(def drunkness (atom 3))
(def social (atom 1))
(def privacy-radius (atom 100))

(def dim {:width 1500 :height 1000})
(def behave-sleep-ms 50)

(defn create-bird [_]
  {:id (rand)
   :x (rand (:width dim))
   :y (rand (:height dim))
   :dx (- (rand @max-speed) (rand @max-speed))
   :dy (- (rand @max-speed) (rand @max-speed))})

(def birds-atom (atom (doall (map create-bird
                                  (range @n-birds)))))

(add-watch n-birds :update-birds-atom
           (fn [_ _ old new]
             (swap! birds-atom #(doall (if (> old new)
                                         (take new %)
                                         (concat % (map create-bird
                                                        (range (- new old)))))))))

;;;;;;;;;;;;;;; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;

(defn vector-length [x y]
  "Length of a vector."
  (Math/sqrt (+ (* x x) (* y y))))

(defn bird-speed [bird]
  (vector-length (:dx bird) (:dy bird)))

(defn distance-between [this that]
  (vector-length (- (:x this) (:x that))
                 (- (:y this) (:y that))))

(defn- neighbors-sorted-by-distance [bird birds]
  "Return list of birds sorted by distance from given bird."
  (sort-by #(vector-length (- (:x bird) (:x %))
                           (- (:y bird) (:y %))) birds))

(defn furthest-bird [bird birds]
  (last (neighbors-sorted-by-distance bird birds)))

(defn closest-bird [bird birds]
  (second (neighbors-sorted-by-distance bird birds)))

;;;;;;;;;;;;;;; BEHAVIOUR ;;;;;;;;;;;;;;;;;;;;;;;

(defn move [bird]
  (assoc bird
    :x (+ (:x bird) (:dx bird))
    :y (+ (:y bird) (:dy bird))))

(defn bounce-world [bird]
  (let [bound (fn [v d dim]
                (cond (> v dim)  (* -1 (Math/abs d))
                      (neg? v)   (Math/abs d)
                      :otherwise d))]
    (assoc bird
      :dx (bound (:x bird) (:dx bird) (:width dim))
      :dy (bound (:y bird) (:dy bird) (:height dim)))))

(defn bounce-others [bird birds]
  (let [neighbor (closest-bird bird birds)
        distance (distance-between bird neighbor)]
    (if (< distance @privacy-radius)
      (let [dx (- (:x neighbor) (:x bird))
            dy (- (:y neighbor) (:y bird))]
        (assoc bird
          :dx (- (:dx bird) (/ dx distance))
          :dy (- (:dy bird) (/ dy distance))))
      bird)))

(defn socialize [bird birds]
  (let [far (furthest-bird bird birds)
        dx (- (:x far) (:x bird))
        dy (- (:y far) (:y bird))
        n (max (Math/abs dx) (Math/abs dy))]
    (assoc bird
      :dx (+ (:dx bird) (* @social (/ dx n)))
      :dy (+ (:dy bird) (* @social (/ dy n))))))

(defn cap-speed [bird]
  (let [speed (bird-speed bird)]
    (if (> speed @max-speed)
      (let [f (/ @max-speed speed)]
        (assoc bird
          :dx (* (:dx bird) f)
          :dy (* (:dy bird) f)))
      bird)))

(defn stumble [bird]
  (assoc bird
    :dx (+ (:dx bird) (- (/ @drunkness 2) (rand @drunkness)))
    :dy (+ (:dy bird) (- (/ @drunkness 2) (rand @drunkness)))))

(defn behave [bird birds]
  (-> bird
      (socialize birds)
      (bounce-others birds)
      stumble
      cap-speed
      bounce-world
      move))

;;;;;;;;;;;;;;; LIFE ;;;;;;;;;;;;;;;

(def running (atom false))

(defn run []
  (when @running (js/setTimeout run behave-sleep-ms))
  (swap! birds-atom (fn [birds] (doall (map #(behave % birds) birds)))))

(defn start []
  (when-not @running
    (reset! running true)
    (run)))

(defn stop []
  (reset! running false))

;;;;;;;;;;;;;;; RENDERING ;;;;;;;;;;;;;;;

(defn bird-component [bird]
  (let [{:keys [id x y dx dy]} bird]
    [[:rect {:key (str "head-" id)
             :x (- x 6)
             :y (- y 6)
             :rx 12
             :ry 12
             :width 12
             :height 12
             :stroke "white"
             }]
     [:line {:key (str "tail-" id),
             :x1 x, :y1 y,
             :x2 (- x (* dx 2)), :y2 (- y (* dy 2)),
             :stroke "black", :stroke-width 4}]]))

(defn birds-component []
  [:g (mapcat bird-component @birds-atom)])

(defn input-range [name atom attrs]
  [:label
   [:span (str " " name " (" @atom ")")]
   [:input (merge {:type "range"
                   :value @atom
                   :on-change #(reset! atom
                                       (js/parseFloat (.-value (.-target %))))}
                  attrs)]])

(defn main-component []
  [:div
   [:svg {:xmlns "http://www.w3.org/2000/svg"
          :version "1.1"
          :viewBox (s/join " " [0 0 (:width dim) (:height dim)])}
    [birds-component]]
   [:div.controles
    (input-range "Amount" n-birds
                 {:min 2 :max 50})
    (input-range "Max. speed" max-speed
                 {:min 0 :max 50})
    (input-range "Social" social
                 {:min 0 :max 5 :step 0.1})
    (input-range "Drunkness" drunkness
                 {:min 0 :max 25})
    (input-range "Privacy" privacy-radius
                 {:min 0 :max 500})
    [:div.stop-start
     (if @running
       [:button {:on-click stop} "stop"]
       [:button {:on-click start} "start"])]]])

(defn ^:export main []
  (let [el (.getElementById js/document "app")]
    (reagent/render-component [main-component] el)
    (start)))
