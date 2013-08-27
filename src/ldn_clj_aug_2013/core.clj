(ns ldn-clj-aug-2013.core)

(defn live-neighbours [board [x y]]
  (->> (for [i [-1 0 1] j [-1 0 1] :when (not= i j 0)]
         (get-in board
                 (into [:state]
                       [(+ x i) (+ y j)])))
       (remove nil?)
       count))

(defn alive? [board coordinate]
  (boolean (condp get (live-neighbours board coordinate)
             #{3} true
             #{2 3} (get-in board (into [:state] coordinate))
             nil)))

(defn next-state [{:keys [width height state] :as board}]
  (assoc board
    :state (into {}
                 (for [x (range width)]
                   (vector x
                           (into #{}
                                 (for [y (range height) :when (alive? board [x y])]
                                   y)))))))

(defn render [board]
  (apply str
         (map (fn [x]
                (str (apply str
                            (if-let [row (get-in board [:state x])]
                              (map #(if (row %) "#" "·")
                                   (range (:width board)))))
                     "\n"))
              (range (:width board)))))

(defn board [& rows]
  (let [width (count (first rows))]
    {:width width
     :height (count rows)
     :state (into {}
                  (map-indexed (fn [idx row]
                                 [idx (into #{}
                                            (for [y (range width) :when (not (zero? (nth row y)))]
                                              y))])
                               rows))}))

(def pulsar
  (vector (repeat 15 0)
          (concat (repeat 3 0) (repeat 3 1) (repeat 3 0) (repeat 3 1) (repeat 3 0))
          (repeat 15 0)

          (concat (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0))
          (concat (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0))
          (concat (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0))

          (concat (repeat 3 0) (repeat 3 1) (repeat 3 0) (repeat 3 1) (repeat 3 0))
          (repeat 15 0)
          (concat (repeat 3 0) (repeat 3 1) (repeat 3 0) (repeat 3 1) (repeat 3 0))

          (concat (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0))
          (concat (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0))
          (concat (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0) (repeat 1 1) (repeat 4 0) (repeat 1 1) (repeat 1 0))

          (repeat 15 0)
          (concat (repeat 3 0) (repeat 3 1) (repeat 3 0) (repeat 3 1) (repeat 3 0))
          (repeat 15 0)))

(defn play!
  [board]
  (loop [[state & states] (iterate next-state board)]
    (println (render state))
    (Thread/sleep 1000)
    (recur states)))

#_(play! (apply board pulsar))
