(ns Player
  (:require [clojure.string :as clj-str]
            [clojure.set :as clj-set])
  (:gen-class))

(defmacro dbg [x]
  `(let [x# ~x]
     (binding [*out* *err*]
       (println "dbg:" '~x "=" x#))
     x#))

(defmacro dbg-v [x]
  `(let [x# ~x]
     (binding [*out* *err*]
       (println "dbg-v:" x#))
     x#))

(def pod-price 20)
(def neutral-zone-owner-id -1)

(defn read-number-input-line []
  (->> (read-line)
       (re-seq #"-?\d+")
       (map #(Integer/parseInt %))))

(defn initialize-platinum-info [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (recur
        (dec i)
        (apply assoc acc (read-number-input-line))))))

(defn initialize-link-info [link-count]
  (loop [i link-count
         acc {}]
    (if (zero? i)
      acc
      (let [[zone1 zone2] (read-number-input-line)]
        (recur
          (dec i)
          (-> (update-in acc [zone1] conj zone2)
              (update-in [zone2] conj zone1)))))))

(defn read-round-platinum-info []
  (->> (read-line)
       Integer/parseInt))

(defrecord ZoneState [zone-id owner-id pod-cnts])

(defn read-round-game-state [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (let [[z-id owner-id & pod-info] (read-number-input-line)
            pod-map (zipmap (range 4)
                            pod-info)]
        (recur
          (dec i)
          (assoc acc z-id (->ZoneState z-id owner-id pod-map)))))))

(defn frontier-distances [link-info my-id game-state]
  (loop [n 1
         acc (->> (vals game-state)
                  (filter (comp (partial not= my-id) :owner-id))
                  (map :zone-id)
                  (into #{})
                  (assoc {} 0))]
    (if-let [n-1 (seq (acc (dec n)))]
      (let [visited (some->> (vals acc)
                             (apply concat)
                             (into #{}))]
        (->> n-1
             (mapcat
               (fn [zone-id]
                 (->> (link-info zone-id)
                      (map game-state)
                      (filter (comp (partial = my-id) :owner-id))
                      (map :zone-id))))
             (into #{})
             (remove visited)
             (assoc acc n)
             (recur (inc n))))
      (reduce
        (fn [m [d ns]]
          (->> (map #(vector % d) ns)
               (into m)))
        {}
        acc))))

(defn zone-value [plat-info link-info depth zone-id]
  (loop [v 0
         d 0
         visited #{}
         q #{zone-id}]
    (if (> d depth)
      v
      (let [modifier (/ (* (inc d) (inc d)))
            next (-> (mapcat link-info q)
                     set
                     (clj-set/difference visited))]
        (recur
          ((fnil + 0)
           (some->> (map plat-info q)
                    (reduce +)
                    (* modifier))
           v)
          (inc d)
          (clj-set/union visited q)
          next)))))

(defn plat-heat-map [plat-info link-info depth]
  (->> (keys plat-info)
       (map #(vector % (zone-value plat-info link-info depth %)))
       (into {})))

(defn halving [n]
  (cond
    (>= 0 n) nil
    (= 1 n) '(1)
    :else (let [half (if (even? n)
                       (/ n 2)
                       (/ (inc n) 2))]
            (cons half (lazy-seq (halving (- n half)))))))

(defn create-score-fn [zone-vals link-info my-id game-state frontier-map]
  (fn [zone-id]
    (let [distance (get frontier-map zone-id (dec Integer/MAX_VALUE))
          enemy-adjacent (some->> (link-info zone-id)
                                  (map game-state)
                                  (map #(dissoc (:pod-cnts %) my-id))
                                  (mapcat vals)
                                  (reduce +))]
      (cond
        (< 1 distance) (/ (inc distance))
        (zero? distance) (+ 51/100 (zone-vals zone-id))
        (pos? enemy-adjacent) (+ 51/100 (zone-vals zone-id))
        :else (/ (inc distance))))))

(defn compute-moves [plat-info link-info my-id game-state frontier-map]
  (let [score-fn (create-score-fn plat-info link-info my-id game-state frontier-map)
        move-fn (fn [zone-id]
                  (let [pod-cnt (get-in game-state [zone-id :pod-cnts my-id])
                        current-val (score-fn zone-id)
                        pod-seq (halving pod-cnt)]
                    (->> (link-info zone-id)
                         (sort-by score-fn (comp unchecked-negate compare))
                         (take-while #(<= current-val (score-fn %)))
                         (map vector pod-seq (repeat zone-id)))))]
    (->> (vals game-state)
         (filter #(pos? (get-in % [:pod-cnts my-id])))
         (mapcat (comp move-fn :zone-id)))))

(defn compute-purchases [plat-info link-info my-id plat game-state frontier-map]
  (let [pod-cnt (quot plat pod-price)
        score-fn (create-score-fn plat-info link-info my-id game-state frontier-map)
        purchases (quot pod-cnt 1)]
    (->> (vals game-state)
         (filter (comp #(or (= my-id %) (= neutral-zone-owner-id %)) :owner-id))
         (map :zone-id)
         (sort-by score-fn (comp unchecked-negate compare))
         (take purchases)
         (map #(vector (quot pod-cnt purchases) %)))))

(defn ->moves-format [moves]
  (if (empty? moves)
    "WAIT"
    (->> (apply concat moves)
         (clj-str/join " "))))

(defn ->purchases-format [purchases]
  (if (empty? purchases)
    "WAIT"
    (->> (apply concat purchases)
         (clj-str/join " "))))

(defn -main [& args]
  (let [[playerCount my-id zone-count link-count] (read-number-input-line)
        plat-info (initialize-platinum-info zone-count)
        link-info (initialize-link-info link-count)
        plat-vals (plat-heat-map plat-info link-info 2)]

    (while true
      (let [platinum (read-round-platinum-info)
            game-state (read-round-game-state zone-count)
            frontier-map (frontier-distances link-info my-id game-state)
            moves (compute-moves plat-vals link-info my-id game-state frontier-map)
            purchases (compute-purchases plat-vals link-info my-id platinum game-state frontier-map)]

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println (->moves-format moves))
        (println (->purchases-format purchases))))))